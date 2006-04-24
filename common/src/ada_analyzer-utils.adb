-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with String_Utils; use String_Utils;

package body Ada_Analyzer.Utils is

   ---------------------
   -- Get_Parent_Type --
   ---------------------

   function Get_Parent_Type
     (Buffer : String; Construct : Construct_Information)
      return String
   is
      Result_Begin, Result_End : Natural := 0;
      Paren_Depth              : Integer := 0;
      Has_Parent               : Boolean := False;

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Used to parse the tokens of the type

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
         Word : constant String := Buffer (Sloc_Start.Index .. Sloc_End.Index);
      begin
         if Paren_Depth = 0 then
            if Entity = Keyword_Text then
               if Word = "access" or else Word = "new" then
                  Has_Parent := True;
               end if;
            elsif Entity = Identifier_Text then
               if Has_Parent then
                  Result_Begin := Sloc_Start.Index;
                  Result_End := Sloc_End.Index;
               end if;

               return True;
            end if;
         end if;

         if Entity = Operator_Text then
            if Word = "(" then
               Paren_Depth := Paren_Depth + 1;
            elsif Word = ")" then
               Paren_Depth := Paren_Depth - 1;
            end if;
         end if;

         return False;
      end Token_Callback;

   begin
      Analyze_Ada_Source
        (Buffer
           (Construct.Sloc_Entity.Index + Construct.Name'Length
            .. Construct.Sloc_End.Index),
         Default_Indent_Parameters,
         Callback => Token_Callback'Unrestricted_Access);

      if Result_Begin = 0 then
         return "";
      else
         return Buffer (Result_Begin .. Result_End);
      end if;
   end Get_Parent_Type;

   ---------------
   -- Is_Access --
   ---------------

   function Is_Access
     (Buffer : String; Construct : Construct_Information)
      return Boolean
   is
      Result : Boolean := False;
      Paren_Depth : Integer := 0;

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean;
      --  Used to parse the tokens of the type

      function Token_Callback
        (Entity         : Language_Entity;
         Sloc_Start     : Source_Location;
         Sloc_End       : Source_Location;
         Partial_Entity : Boolean) return Boolean
      is
         pragma Unreferenced (Partial_Entity);
         Word : constant String := Buffer (Sloc_Start.Index .. Sloc_End.Index);
      begin
         if Paren_Depth = 0 and then Entity = Keyword_Text then
            if Word = "access" then
               Result := True;
               return True;
            end if;
         end if;

         if Entity = Operator_Text then
            if Word = "(" then
               Paren_Depth := Paren_Depth + 1;
            elsif Word = ")" then
               Paren_Depth := Paren_Depth - 1;
            end if;
         end if;

         return False;
      end Token_Callback;

   begin
      Analyze_Ada_Source
        (Buffer (Construct.Sloc_Start.Index .. Construct.Sloc_End.Index),
         Default_Indent_Parameters,
         Callback => Token_Callback'Unrestricted_Access);

      return Result;
   end Is_Access;

   ------------------
   -- Get_Typename --
   ------------------

   function Get_Typename
     (Buffer : String; Construct : Construct_Information)
      return Composite_Identifier
   is
      Index : Natural := Construct.Sloc_Start.Index;
   begin
      Skip_To_Char (Buffer, Index, ':');
      Index := Index + 1;
      Skip_Blanks (Buffer, Index);

      return To_Composite_Identifier
        (Buffer (Index .. Construct.Sloc_End.Index));
   end Get_Typename;

end Ada_Analyzer.Utils;
