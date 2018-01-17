------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with String_Utils;            use String_Utils;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Symbols;        use GNATCOLL.Symbols;

package body Syntax_Diff is

   Move_Threshold : constant := 20;
   --  Only consider constructs that are moved by at least this amount of lines

   procedure Add_Result
     (Results       : in out Result_Link;
      Kind          : Diff_Kind;
      Construct     : Construct_Access;
      Category      : Language_Category;
      New_Construct : Construct_Access := null);
   --  Append a result to the list of results

   function Profile_Image (Profile : Symbol) return String;
   --  Return a string suitable for printing Profile.

   function Image (Info : Construct_Access) return String;
   --  Return a string suitable for printing Info.

   function Image (Category : Language_Category) return String;
   --  Return a string suitable for printing Category.

   procedure Analyze_Construct
     (Info    : Construct_Access;
      C2      : Construct_List;
      Results : in out Result_Link);
   --  Analyze C1.Current based on C2 contents:
   --  - ignore if it is not a construct suitable for analyze
   --  - mark as removed
   --  - mark as moved

   procedure Find_Construct
     (C             : Construct_Access;
      List          : Construct_List;
      Result        : out Construct_Access;
      Profile_Match : out Boolean);
   --  Find in List a construct represented by C.
   --  Result is null is no matching construct was found.
   --  Profile_Match is set to true is the profile of the construct found
   --  matches exactly. Not relevant if Result is null.

   function Is_Equal (C1, C2 : Construct_Access) return Boolean;
   --  Return True if C1 and C2 are the same constructs.

   function Profile_Equal (C1, C2 : Construct_Access) return Boolean;
   --  Return True is C1 and C2 have the same profile.
   --  C1 and C2 are non null.

   function Name_Equal (C1, C2 : Construct_Access) return Boolean;
   --  Return True is C1 and C2 have the same name.
   --  C1 and C2 are non null.

   function Within_Scope (C, Scope : Construct_Access) return Boolean;
   --  Return True if C is contained within Scope.

   function Upper_Scope (C : Construct_Access) return Construct_Access;
   --  Return the upper scope of C.
   --  C is assumed to be non null.

   function Same_Scope (C1, C2 : Construct_Access) return Boolean;
   --  Return True if C1 and C2 are within the same scope.

   function Filter_Category (Category : Language_Category) return Boolean;
   --  Return True if Category should be taken into account.

   ----------------
   -- Add_Result --
   ----------------

   procedure Add_Result
     (Results       : in out Result_Link;
      Kind          : Diff_Kind;
      Construct     : Construct_Access;
      Category      : Language_Category;
      New_Construct : Construct_Access := null)
   is
      Link  : Result_Link;
      Prev  : Result_Link;
      R     : Result_Link;
      Index : constant Natural := Construct.Info.Sloc_Start.Index;

   begin
      Link := new Result_Record (Kind);
      Link.Construct := Construct;
      Link.Category := Category;
      Link.New_Construct := New_Construct;

      --  Insert Link at the right position:

      R := Results;

      loop
         exit when R = null;

         if Index < R.Construct.Info.Sloc_Start.Index then
            if Prev = null then
               Results := Link;
            else
               Prev.Next := Link;
            end if;

            Link.Next := R;
            return;
         end if;

         Prev := R;
         R := R.Next;
      end loop;

      if Prev = null then
         Results := Link;
      else
         Prev.Next := Link;
      end if;
   end Add_Result;

   ---------------------
   -- Filter_Category --
   ---------------------

   function Filter_Category (Category : Language_Category) return Boolean is
   begin
      case Category is
         when Namespace_Category | Subprogram_Category | Dependency_Category |
              Data_Type_Category | Cat_Representation_Clause |
              Cat_Local_Variable | Cat_Parameter | Cat_Discriminant |
              Cat_Literal | Cat_Field |
              Cat_Exception_Handler | Construct_Category | Cat_Pragma |
              Cat_Aspect
         =>
            return True;

         when Cat_Unknown | Cat_Custom =>
            return False;
      end case;
   end Filter_Category;

   --------------------
   -- Find_Construct --
   --------------------

   procedure Find_Construct
     (C             : Construct_Access;
      List          : Construct_List;
      Result        : out Construct_Access;
      Profile_Match : out Boolean)
   is
      Info      : Construct_Access;
      Candidate : Construct_Access;
   begin
      Profile_Match := True;
      Info := List.First;

      loop
         exit when Info = null;

         if Info.Info.Category = C.Info.Category
           and then Info.Info.Is_Declaration = C.Info.Is_Declaration
           and then Name_Equal (Info, C)
           and then Same_Scope (Info, C)
         then
            if Profile_Equal (Info, C) then
               Result := Info;
               return;
            end if;

            if Candidate = null then
               Candidate := Info;
            end if;
         end if;

         Info := Info.Next;
      end loop;

      Result := Candidate;

      if Candidate /= null then
         Profile_Match := False;
      end if;
   end Find_Construct;

   -------------------
   -- Profile_Equal --
   -------------------

   function Profile_Equal (C1, C2 : Construct_Access) return Boolean is
   begin
      if C1.Info.Profile = No_Symbol then
         return C2.Info.Profile = No_Symbol;
      elsif C2.Info.Profile = No_Symbol then
         return False;
      else
         return Reduce (Get (C1.Info.Profile).all) =
                Reduce (Get (C2.Info.Profile).all);
      end if;
   end Profile_Equal;

   ----------------
   -- Name_Equal --
   ----------------

   function Name_Equal (C1, C2 : Construct_Access) return Boolean is
   begin
      return C1.Info.Name = C2.Info.Name;
   end Name_Equal;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (C1, C2 : Construct_Access) return Boolean is
   begin
      if C1 = null then
         return C2 = null;
      elsif C2 = null then
         return False;
      elsif C1.Info.Category = Cat_Unknown
        or else C2.Info.Category = Cat_Unknown
      then
         return True;
      else
         return C1.Info.Category = C2.Info.Category
           and then Name_Equal (C1, C2)
           and then C1.Info.Is_Declaration = C2.Info.Is_Declaration
           and then Profile_Equal (C1, C2);
      end if;
   end Is_Equal;

   ------------------
   -- Within_Scope --
   ------------------

   function Within_Scope (C, Scope : Construct_Access) return Boolean is
   begin
      return C.Info.Sloc_Start.Index > Scope.Info.Sloc_Start.Index
        and then C.Info.Sloc_End.Index < Scope.Info.Sloc_End.Index;
   end Within_Scope;

   -----------------
   -- Upper_Scope --
   -----------------

   function Upper_Scope (C : Construct_Access) return Construct_Access is
      Upper : Construct_Access := C.Next;
   begin
      loop
         exit when Upper = null or else Within_Scope (C, Upper);

         Upper := Upper.Next;
      end loop;

      return Upper;
   end Upper_Scope;

   ----------------
   -- Same_Scope --
   ----------------

   function Same_Scope (C1, C2 : Construct_Access) return Boolean is
      S1, S2 : Construct_Access;
   begin
      S1 := C1;
      S2 := C2;

      loop
         S1 := Upper_Scope (S1);
         S2 := Upper_Scope (S2);

         if not Is_Equal (S1, S2) then
            return False;
         end if;

         exit when S1 = null;
      end loop;

      return True;
   end Same_Scope;

   -----------
   -- Image --
   -----------

   function Image (Category : Language_Category) return String is
      Kind   : constant String := Language_Category'Image (Category);
      Result : String := To_Lower (Kind (5 .. Kind'Last));
   begin
      for J in Result'Range loop
         if Result (J) = '_' then
            Result (J) := ' ';
         end if;
      end loop;

      return Result;
   end Image;

   function Image (Info : Construct_Access) return String is
      function Loc_Info return String;
      --  Return location information

      --------------
      -- Loc_Info --
      --------------

      function Loc_Info return String is
      begin
         return "at " &
                Image (Info.Info.Sloc_Start.Line) & ":" &
                Image (Info.Info.Sloc_Start.Column);
      end Loc_Info;

   begin
      if Info.Info.Name = No_Symbol then
         return Loc_Info;
      elsif Info.Info.Is_Declaration
        and then Info.Info.Category in Enclosing_Entity_Category
      then
         return Get (Info.Info.Name).all & " (spec) " & Loc_Info;
      else
         return Get (Info.Info.Name).all & " " & Loc_Info;
      end if;
   end Image;

   -------------------
   -- Profile_Image --
   -------------------

   function Profile_Image (Profile : Symbol) return String is
      Line_Length : constant := 72;
   begin
      if Profile = No_Symbol then
         return "<none>";
      else
         declare
            Prof : constant String := Reduce (Get (Profile).all);
         begin
            if Prof'Length < Line_Length then
               return Prof;
            else
               return Get (Profile).all;
            end if;
         end;
      end if;
   end Profile_Image;

   -----------------------
   -- Analyze_Construct --
   -----------------------

   procedure Analyze_Construct
     (Info    : Construct_Access;
      C2      : Construct_List;
      Results : in out Result_Link)
   is
      Info2   : Construct_Access;
      Upper   : Construct_Access;
      Profile : Boolean;

   begin
      if Filter_Category (Info.Info.Category) then
         Find_Construct (Info, C2, Info2, Profile);

         if Info2 = null then
            --  If upper scope has already been analyzed and marked as
            --  removed, no need to print anything.

            Upper := Upper_Scope (Info);

            if Upper = null or else Upper.Info.Category /= Cat_Unknown then
               Add_Result (Results, Removed, Info, Info.Info.Category);
            end if;

            Info.Info.Category := Cat_Unknown;

         else
            --  Mark Info2 as analyzed/found

            Info2.Info.Category := Cat_Unknown;

            --  Look for moved entities:
            --  Only consider enclosing entities, to avoid too many false
            --  matches; Also ignore small moves.

            if Info.Info.Category in Enclosing_Entity_Category
              and then abs
                (Info.Info.Sloc_Start.Line - Info2.Info.Sloc_Start.Line) >
                 Move_Threshold
              and then not
                (Is_Equal (Info.Prev, Info2.Prev)
                 and then Is_Equal (Info.Next, Info2.Next))
            then
               Add_Result (Results, Moved, Info, Info.Info.Category, Info2);
            end if;

            if not Profile then
               Add_Result
                 (Results, Profile_Changed, Info, Info.Info.Category, Info2);
            end if;
         end if;
      end if;
   end Analyze_Construct;

   -----------------
   -- Syntax_Diff --
   -----------------

   function Syntax_Diff
     (Old_Constructs, New_Constructs : Construct_List) return Result_Link
   is
      Info    : Construct_Access;
      --  Upper : Construct_Access;
      Results : Result_Link;

   begin
      --  First pass to find the removed and moved constructs.
      --  Traverse the list from the end so that inner removed constructs can
      --  be suppressed.

      Info := Old_Constructs.Last;

      loop
         exit when Info = null;

         Analyze_Construct (Info, New_Constructs, Results);
         Info := Info.Prev;
      end loop;

      --  Second pass: find new constructs

      Info := New_Constructs.First;

      loop
         exit when Info = null;

         if Filter_Category (Info.Info.Category) then
            --  Upper := Upper_Scope (Info);
            --  if Upper = null or else Upper.Category /= Cat_Unknown then
            Add_Result (Results, Added, Info, Info.Info.Category);
         end if;

         Info := Info.Next;
      end loop;

      return Results;
   end Syntax_Diff;

   -------------------
   -- Print_Results --
   -------------------

   procedure Print_Results (Results : Result_Link) is
      R : Result_Link := Results;
   begin
      loop
         exit when R = null;

         case R.Kind is
            when Moved =>
               Put_Line ("* " & Image (R.Category) & " " &
                         Image (R.Construct) & " moved to " &
                         Image (R.New_Construct.Info.Sloc_Start.Line) & ":" &
                         Image (R.New_Construct.Info.Sloc_Start.Column));

            when Profile_Changed =>
               Put_Line ("* profile changed for " & Image (R.Construct) & ":");
               Put_Line ("< old: " & Profile_Image (R.Construct.Info.Profile));
               Put_Line
                  ("> new: " & Profile_Image (R.New_Construct.Info.Profile));

            when Added =>
               Put_Line ("+ " & Image (R.Category) & " " &
                         Image (R.Construct));

            when Removed =>
               Put_Line ("- " & Image (R.Category) & " " &
                         Image (R.Construct));
         end case;

         R := R.Next;
      end loop;
   end Print_Results;

end Syntax_Diff;
