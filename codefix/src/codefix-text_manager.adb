-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Regpat;           use GNAT.Regpat;
with Basic_Types;           use Basic_Types;
with String_Utils;          use String_Utils;

package body Codefix.Text_Manager is

   -----------------
   -- Compare_Pkg --
   -----------------

   function Compare_Pkg (Pkg_1, Pkg_2 : String) return Boolean is
      Pkg_1_Lower : String := Pkg_1;
      Pkg_2_Lower : String := Pkg_2;
   begin
      Lower_Case (Pkg_1_Lower);
      Lower_Case (Pkg_2_Lower);

      if Pkg_1'Length < Pkg_2'Length then
         return Pkg_1_Lower = Pkg_2_Lower
           (Pkg_2'Last - Pkg_1'Length + 1 .. Pkg_2'Last);
      else
         return Pkg_2_Lower = Pkg_1_Lower
           (Pkg_1'Last - Pkg_2'Length + 1 .. Pkg_1'Last);
      end if;
   end Compare_Pkg;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Str : String) return Boolean is
      Blank_Str : constant String (Str'First .. Str'Last) := (others => ' ');
   begin
      return Str = Blank_Str;
   end Is_Blank;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Text_Cursor) return Boolean is
   begin
      return Left.Line < Right.Line
        or else (Left.Line = Right.Line and then Left.Col < Right.Col);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left < Right) and then Left /= Right;
   end ">";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : File_Cursor) return Boolean is
   begin

      if Left.File_Name /= null xor Right.File_Name /= null then
         return False;
      else
         return Left.Line = Right.Line
           and then Left.Col = Right.Col
           and then ((Left.File_Name = null and then Right.File_Name = null)
                     or else (Left.File_Name.all = Right.File_Name.all));
      end if;

   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : File_Cursor) return Boolean is
   begin
      return Left.File_Name.all < Right.File_Name.all
        or else (Left.File_Name.all = Right.File_Name.all
                 and then Text_Cursor (Left) < Text_Cursor (Right));
   end "<";

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Navigator_Abstr) is
   begin
      Free (This.Files.all);
      Free (This.Files);
   end Free;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Current_Text           : Text_Navigator_Abstr;
      Cursor                 : File_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
     return Construct_Information is
   begin
      return Get_Unit
        (Get_File (Current_Text, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         Position,
         Category_1,
         Category_2);
   end Get_Unit;

   -----------------
   -- Search_Body --
   -----------------

   function Search_Body
     (Current_Text : Text_Navigator_Abstr;
      File_Name    : String;
      Spec         : Construct_Information) return Construct_Information is
   begin
      return Search_Body (Get_File (Current_Text, File_Name).all, Spec);
   end Search_Body;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural) return String is
   begin
      return Get
        (Get_File (This, Cursor.File_Name.all).all, Text_Cursor (Cursor), Len);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return String is
   begin
      return Get_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor));
   end Get_Line;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (This : Text_Navigator_Abstr;
      File_Name : String) return Dynamic_String is
   begin
      return Read_File (Get_File (This, File_Name).all);
   end Read_File;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : String) return Ptr_Text
   is
      Iterator    : Text_List.List_Node := First (This.Files.all);
      New_Text    : Ptr_Text;
      New_Buffer  : Extended_Line_Buffer;
      Ignore      : Natural;
      Buffer      : Dynamic_String;

      procedure Display_Constructs;
      --  Debug procedure used to display the contents of New_Text

      procedure Display_Constructs is
         Current : Construct_Access := New_Text.Tokens_List.First;
      begin
         while Current /= null loop
            if Current.Name /= null then
               Put (Current.Name.all & ": ");
            end if;

            Put (Current.Category'Img);
            Put (", " & Current.Sloc_End.Line'Img & ", ");
            Put (Current.Sloc_End.Column'Img);
            Put (" (");

            if Current.Is_Declaration then
               Put ("spec");
            else
               Put ("body");
            end if;

            Put (")");
            New_Line;
            Current := Current.Next;
         end loop;
      end Display_Constructs;

   begin
      while Iterator /= Text_List.Null_Node loop
         if Get_File_Name (Data (Iterator).all) = Name then
            return Data (Iterator);
         end if;

         Iterator := Next (Iterator);
      end loop;

      New_Text := New_Text_Interface (This);

      Append (This.Files.all, New_Text);

      New_Text.File_Name := new String'(Name);
      Initialize (New_Text.all, Name);
      Buffer := Read_File (New_Text.all);

      Analyze_Ada_Source
        (Buffer => Buffer.all,
         New_Buffer => New_Buffer,
         Indent_Params => Default_Indent_Parameters,
         Reserved_Casing  => Unchanged,
         Ident_Casing => Unchanged,
         Format_Operators => False,
         Indent => False,
         Constructs => New_Text.Tokens_List,
         Current_Indent => Ignore,
         Prev_Indent => Ignore,
         Callback => null);

      pragma Debug (Display_Constructs);

      Free (Buffer);

      return New_Text;
   end Get_File;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String) is
   begin
      Replace
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         Len,
         New_Value);
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out Text_Navigator_Abstr;
      Cursor      : File_Cursor'Class;
      New_Line    : String) is
   begin
      Add_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         New_Line);
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) is
   begin
      Delete_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor));
   end Delete_Line;

   ----------------
   -- Get_Entity --
   ----------------

   procedure Get_Entity
     (This : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor : File_Cursor'Class)
   is
      Unit_Info, Body_Info : Construct_Information;
      Line_Cursor          : File_Cursor := File_Cursor (Cursor);
   begin
      Line_Cursor.Col := 1;

      Unit_Info := Get_Unit (Current_Text, Cursor);

      if Unit_Info.Is_Declaration then
         Body_Info := Search_Body
           (Current_Text,
            Cursor.File_Name.all,
            Unit_Info);
         for J in Body_Info.Sloc_Start.Line .. Body_Info.Sloc_End.Line loop
            Line_Cursor.Line := J;
            Get_Line (Current_Text, Line_Cursor, This);
         end loop;
      end if;

      for J in Unit_Info.Sloc_Start.Line .. Unit_Info.Sloc_End.Line loop
         Line_Cursor.Line := J;
         Get_Line (Current_Text, Line_Cursor, This);
      end loop;
   end Get_Entity;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural is
   begin
      return Line_Length
        (Get_File (This, Cursor.File_Name.all).all,
         File_Cursor (Cursor));
   end Line_Length;

   ------------
   -- Update --
   ------------

   procedure Commit (This : Text_Navigator_Abstr) is
      Iterator : Text_List.List_Node := First (This.Files.all);
   begin
      while Iterator /= Text_List.Null_Node loop
         Commit (Data (Iterator).all);
         Iterator := Next (Iterator);
      end loop;
   end Commit;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This         : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True)
   return File_Cursor'Class is
   begin
      return Search_String
        (Get_File (This, Cursor.File_Name.all).all,
         File_Cursor (Cursor),
         Searched,
         Step,
         Jump_String);
   end Search_String;

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String;
      Category  : Language_Category;
      Name      : String) return Construct_Information is
   begin
      return Search_Unit (Get_File (This, File_Name).all, Category, Name);
   end Search_Unit;

   --------------
   -- Line_Max --
   --------------

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String) return Natural is
   begin
      return Line_Max (Get_File (This, File_Name).all);
   end Line_Max;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Interface) is
   begin
      Free (This.Tokens_List.all);
      Free (This.Tokens_List);
      Free (This.File_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Text) is
      procedure Delete is new
         Ada.Unchecked_Deallocation (Text_Interface'Class, Ptr_Text);
   begin
      Free (This.all);
      Delete (This);
   end Free;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Current_Text           : Text_Interface;
      Cursor                 : Text_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
     return Construct_Information
   is

      function Nearer return Boolean;
      --  Return True when Current_Info is nearer Cursor than Info_Saved.

      Current_Info : Construct_Access;
      Info_Saved   : Construct_Access;

      function Nearer return Boolean is
         D_Current_Line, D_Current_Col : Integer;
         D_Old_Line, D_Old_Col : Integer;
      begin
         D_Current_Line := Current_Info.Sloc_Start.Line - Cursor.Line;
         D_Current_Col := Current_Info.Sloc_Start.Column - Cursor.Col;

         case Position is
            when Before =>
               if D_Current_Line > 0
                 or else (D_Current_Line = 0 and then D_Current_Col > 0)
               then
                  return False;
               end if;
            when After =>
               if D_Current_Line > 0
                 or else (D_Current_Line = 0 and then D_Current_Col > 0)
               then
                  return False;
               end if;
            when others =>
               return False;
         end case;

         if Info_Saved = null then return True; end if;

         D_Old_Line := Info_Saved.Sloc_Start.Line - Cursor.Line;
         D_Old_Col := Info_Saved.Sloc_Start.Column - Cursor.Col;

         return (abs D_Old_Line) >  (abs D_Current_Line) or else
           (D_Old_Line = D_Current_Line and then
            (abs D_Old_Col) > (abs D_Current_Col));

      end Nearer;

      --  begin of Get_Unit

   begin
      Current_Info := Current_Text.Tokens_List.First;

      while Current_Info /= null loop

         if Category_1 = Cat_Unknown
           or else Current_Info.Category = Category_1
           or else Current_Info.Category = Category_2
         then
            if (Current_Info.Sloc_Start.Line = Cursor.Line
             and then Current_Info.Sloc_Start.Column = Cursor.Col)
              or else (Current_Info.Sloc_Entity.Line = Cursor.Line
                    and then Current_Info.Sloc_Entity.Column = Cursor.Col)
            then
               return Current_Info.all;
            elsif Position /= Specified and then Nearer then
               Info_Saved := Current_Info;
            end if;
         end if;

         Current_Info := Current_Info.Next;
      end loop;

      if Info_Saved /= null then
         return Info_Saved.all;
      end if;

      Raise_Exception
        (Codefix_Panic'Identity,
         "Cursor given is not at the beginning of a unit.");
   end Get_Unit;

   -----------------
   -- Search_Body --
   -----------------

   --  Assertion : The last unit can never be the unit looked for
   function Search_Body
     (Current_Text : Text_Interface;
      Spec         : Construct_Information) return Construct_Information
   is
      function Normalize (Str : Basic_Types.String_Access) return String;
      --  Change the string in order to make comparaisons between lists of
      --  parameters.

      procedure Seeker (Stop : Source_Location);
      --  Recursivly scan a token list in order to found the declaration what
      --  correpond with a body. Stop is the beginning of the current block.

      Current_Info : Construct_Access;
      Found        : Boolean := False;
      Result       : Construct_Access;

      ---------------
      -- Normalize --
      ---------------

      function Normalize (Str : Basic_Types.String_Access) return String is
      begin
         if Str /= null then
            return Reduce (Str.all);
         else
            return "";
         end if;
      end Normalize;

      ------------
      -- Seeker --
      ------------

      procedure Seeker (Stop : Source_Location) is
         Current_Result : Construct_Access;
         New_Sloc       : Source_Location;
      begin
         while Current_Info /= null loop

            if Current_Info.Category not in Construct_Category then

               --  Is it the end of the scope ?
               if Current_Info.Sloc_End.Line < Stop.Line
                 or else (Current_Info.Sloc_End.Line = Stop.Line
                       and then Current_Info.Sloc_End.Column < Stop.Column)
               then
                  return;
               end if;

               --  Does this body have the rigth profile ?
               if not Current_Info.Is_Declaration and then
               Current_Info.Name.all = Spec.Name.all and then
               Normalize (Current_Info.Profile) = Normalize (Spec.Profile)
               then
                  Result := Current_Info;
               end if;


               --  Is it the beginnig of a scope ?
               if not Current_Info.Is_Declaration and then
               Current_Info.Category in Enclosing_Entity_Category
               then
                  Current_Result := Result;
                  New_Sloc := Current_Info.Sloc_Start;
                  Current_Info := Current_Info.Prev;
                  Current_Result := Result;
                  Seeker (New_Sloc);

                  if Found then
                     return;
                  else
                     Result := Current_Result;
                  end if;

                  --  Is this spec the rigth one ?
               elsif Current_Info.Is_Declaration and then
                 Current_Info.Name.all = Spec.Name.all and then
                 Current_Info.Sloc_Start = Spec.Sloc_Start and then
                 Normalize (Current_Info.Profile) = Normalize (Spec.Profile)
               then
                  Found := True;
                  return;
               else
                  Current_Info := Current_Info.Prev;
               end if;

            else

               Current_Info := Current_Info.Prev;

            end if;

         end loop;
      end Seeker;

   begin
      Current_Info := Current_Text.Tokens_List.Last;
      Seeker (Current_Info.Sloc_Start);
      return Result.all;
   end Search_Body;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (This : Text_Interface) return String is
   begin
      return This.File_Name.all;
   end Get_File_Name;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural is
   begin
      return Get_Line (This, Cursor)'Length;
   end Line_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This         : Text_Interface'Class;
      Cursor       : Text_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True)
     return File_Cursor'Class is

      Last, Increment    : Integer;
      Ext_Red            : Extract_Line;
      New_Cursor, Result : File_Cursor;
      Old_Col            : Integer;

   begin
      New_Cursor := File_Cursor (Cursor);

      case Step is
         when Normal_Step =>
            Last := Line_Max (This);
            Increment := 1;
            Get_Line (This, New_Cursor, Ext_Red);
         when Reverse_Step =>
            Last := 1;
            Increment := -1;
            Old_Col := New_Cursor.Col;
            New_Cursor.Col := 1;
            Get (This, New_Cursor, Old_Col, Ext_Red);
            New_Cursor.Col := Old_Col;
      end case;


      loop
         Result := File_Cursor (Search_String
                                  (Ext_Red,
                                   New_Cursor,
                                   Searched,
                                   Step,
                                   Jump_String));

         if Result /= Null_File_Cursor then
            Result := Clone (Result);
            Free (Ext_Red);
            return Result;
         end if;

         exit when New_Cursor.Line = Last;

         New_Cursor.Col := 1;
         New_Cursor.Line := New_Cursor.Line + Increment;
         Free (Ext_Red);
         Get_Line (This, New_Cursor, Ext_Red);

         if Step = Reverse_Step then
            New_Cursor.Col := 0;
         end if;

      end loop;

      Free (Ext_Red);
      return Null_File_Cursor;
   end Search_String;

   -----------------
   -- Search_Unit --
   -----------------

   function Search_Unit
     (This     : Text_Interface'Class;
      Category : Language_Category;
      Name     : String) return Construct_Information is

      Current_Info : Construct_Access;

   begin

      Current_Info := This.Tokens_List.First;

      while Current_Info /= null loop
         if Current_Info.Category = Category
           and then Compare_Pkg (Current_Info.Name.all, Name)
         then
            return Current_Info.all;
         end if;
         Current_Info := Current_Info.Next;
      end loop;

      return (Category        => Cat_Unknown,
              Name            => null,
              Profile         => null,
              Sloc_Start      => (0, 0, 0),
              Sloc_Entity     => (0, 0, 0),
              Sloc_End        => (0, 0, 0),
              Is_Declaration  => False,
              Prev            => null,
              Next            => null);

   end Search_Unit;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out File_Cursor) is
   begin
      Free (This.File_Name);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : File_Cursor) return File_Cursor is
      New_Cursor : File_Cursor := This;
   begin
      New_Cursor.File_Name := Clone (This.File_Name);
      return New_Cursor;
   end Clone;

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Extract_Line) return Boolean is
   begin
      return Left.Cursor = Right.Cursor
        and then Left.Context = Right.Context
        and then (Left.Context = Line_Deleted
                  or else Left.Content.all = Right.Content.all);
   end "=";

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out Extract_Line; Value : Extract_Line) is
   begin
      This.Context := Value.Context;
      Free (This.Cursor);
      This.Cursor := Clone (Value.Cursor);
      Assign (This.Content, Value.Content);
   end Assign;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract_Line) is
   begin
      Free (This.Content);
      Free (This.Cursor);
      if This.Next /= null then
         Free (This.Next.all);
         Free (This.Next);
      end if;
   end Free;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (This : Extract_Line) return Line_Context is
   begin
      return This.Context;
   end Get_Context;

   ----------
   -- Next --
   ----------

   function Next (This : Extract_Line) return Ptr_Extract_Line is
   begin
      return This.Next;
   end Next;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This : Extract_Line) return String is
   begin
      return This.Content.all;
   end Get_String;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor (This : Extract_Line) return File_Cursor'Class is
   begin
      return This.Cursor;
   end Get_Cursor;

   ------------
   -- Length --
   ------------

   function Length (This : Extract_Line) return Natural is
   begin
      if This.Next /= null then
         return Length (This.Next.all) + 1;
      else
         return 1;
      end if;
   end Length;

   ------------
   -- Update --
   ------------

   procedure Commit
     (This         : Extract_Line;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer)
   is
      Real_Cursor : File_Cursor := This.Cursor;
   begin
      Real_Cursor.Line := Real_Cursor.Line + Offset_Line;

      case This.Context is
         when Line_Created =>
            Add_Line
              (Current_Text,
               Real_Cursor,
               This.Content.all);
            Offset_Line := Offset_Line + 1;

         when Line_Deleted =>
            Delete_Line
              (Current_Text,
               Real_Cursor);
            Offset_Line := Offset_Line - 1;

         when others =>
            Replace
             (Current_Text,
              Real_Cursor,
              This.Original_Length,
              This.Content.all);
      end case;
   end Commit;

   -----------
   -- Clone --
   -----------

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean) return Extract_Line
   is
      New_Line : Extract_Line := This;
   begin

      New_Line.Cursor := Clone (New_Line.Cursor);
      New_Line.Content := new String'(New_Line.Content.all);

      if Recursive and then New_Line.Next /= null then
         New_Line.Next := new Extract_Line'(Clone (New_Line.Next.all, True));
      else
         New_Line.Next := null;
      end if;

      return New_Line;
   end Clone;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Natural;
      Format : String) return Natural
   is
      Matches    : Match_Array (1 .. 1);
      Matcher    : constant Pattern_Matcher := Compile (Format);
      Str_Parsed : constant String := This.Content.all;

   begin
      Match (Matcher, Str_Parsed (Col .. Str_Parsed'Last), Matches);

      if Matches (1) = No_Match then
         Raise_Exception (Text_Manager_Error'Identity, "pattern '" & Format &
                          "' from col" & Integer'Image (Col) & " in '" &
                          Str_Parsed & "' can't be found");

      else
         return Matches (1).Last - Col + 1;
      end if;
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This     : Extract_Line;
      Cursor   : File_Cursor'Class;
      Searched : String;
      Step     : Step_Way := Normal_Step;
      Jump_String  : Boolean := True) return File_Cursor'Class
   is
      pragma Unreferenced (Jump_String);

      Result : File_Cursor := File_Cursor (Cursor);

   begin
      if Result.Col = 0 then
         Result.Col := This.Content'Last;
      end if;

      case Step is
         when Normal_Step =>
            for J in Result.Col .. This.Content'Last - Searched'Last + 1 loop
               if This.Content (J .. J + Searched'Last - 1) = Searched then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;

         when Reverse_Step =>
            for J in reverse This.Content'First ..
              Result.Col - Searched'Last + 1
            loop
               if This.Content (J .. J + Searched'Last - 1) = Searched then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;
      end case;

      return Null_File_Cursor;
   end Search_String;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line) is
   begin
      Destination :=
        (Context         => Original_Line,
         Cursor          => Clone (Cursor),
         Original_Length => Len,
         Content         => new String'(Get (This, Cursor, Len)),
         Next            => null,
         Coloration      => True);
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line) is

      Str : Dynamic_String;

   begin
      Str := new String'(Get_Line (This, Cursor));
      Destination :=
        (Context         => Original_Line,
         Cursor          => File_Cursor (Cursor),
         Original_Length => Str.all'Last,
         Content         => Str,
         Next            => null,
         Coloration      => True);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line) is

      Str : Dynamic_String;

   begin
      Str := new String'(Get_Line (This, Cursor));
      Destination :=
        (Context         => Original_Line,
         Cursor          => Clone (Cursor),
         Original_Length => Str'Last,
         Content         => Str,
         Next            => null,
         Coloration      => True);
   end Get_Line;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This : Extract_Line; Detail : Boolean := True) return String
   is
      pragma Unreferenced (Detail);
   begin
      return This.Content.all;
   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class) return String
   is
      Old_Extract : Extract;

   begin
      case This.Context is
         when Original_Line | Line_Modified =>
            --  Simplifier l'appel de GET pour n'avoir qu'une ligne et pas
            --  un extrait
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;

         when Line_Created =>
            null;
         when Line_Deleted =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;

      end case;
      return "";
   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length
     (This      : Extract_Line;
      Recursive : Boolean := False) return Natural is
      Total : Natural := 0;

      Buffer : constant String := Get_New_Text (This);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_New_Text_Length (This.Next.all, True);
      end if;
      return Total + Buffer'Length;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This      : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class;
      Recursive : Boolean := False) return Natural is

      Total  : Natural := 0;
      Buffer : constant String := Get_Old_Text (This, Current_Text);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_Old_Text_Length (This.Next.all, Current_Text, True);
      end if;
      return Total + Buffer'Length;
   end Get_Old_Text_Length;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This          : in out Ptr_Extract_Line;
      Prev          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural) is

      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;


   begin

      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;

      if Prev = null then

         for I in 1 .. Size loop
            exit when Line_Cursor.Line = 1;
            if Current_Line.Context /= Line_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;
            exit when Line_Cursor.Line = 0;
            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else

         for I in 1 .. Size loop
            if Current_Line.Context /= Line_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;
            exit when Prev.Cursor.File_Name.all = This.Cursor.File_Name.all
              and then (Prev.Cursor.Line + 1 = This.Cursor.Line
                        or else Prev.Cursor.Line = This.Cursor.Line);
            exit when Line_Cursor.Line = 0;
            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Prev.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      end if;

      Free (New_Line);
      Extend_Before (This.Next, This, Current_Text, Size);
      This := Current_Line;

   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural) is

      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;
      End_Of_File            : Natural;

   begin

      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;
      End_Of_File := Line_Max (Current_Text, Line_Cursor.File_Name.all);

      if This.Next = null then

         for I in 1 .. Size loop
            Line_Cursor.Line := Line_Cursor.Line + 1;
            exit when Line_Cursor.Line > End_Of_File;
            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else

         for I in 1 .. Size loop
            exit when Current_Line.Cursor.File_Name.all =
              Current_Line.Next.Cursor.File_Name.all
              and then (Current_Line.Cursor.Line + 1 =
                Current_Line.Next.Cursor.Line
                or else Current_Line.Next.Cursor.Line =
                  Current_Line.Cursor.Line);
            Line_Cursor.Line := Line_Cursor.Line + 1;
            exit when Line_Cursor.Line > End_Of_File;
            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      end if;

      Free (New_Line);

      Extend_After (Current_Line.Next, Current_Text, Size);

   end Extend_After;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (This        : in out Extract_Line;
      New_String  : String;
      First, Last : Natural := 0)
   is
      First_Used, Last_Used : Natural;
   begin
      if First = 0 then
         First_Used := 1;
      else
         First_Used := First;
      end if;

      if Last = 0 then
         Last_Used := This.Content'Last;
      else
         Last_Used := Last;
      end if;

      This.Context := Line_Modified;
      Assign
        (This.Content, This.Content (This.Content'First .. First_Used - 1) &
           New_String &
             This.Content (Last_Used + 1 .. This.Content'Last));
   end Set_String;

   --------------------
   -- Set_Coloration --
   --------------------

   procedure Set_Coloration (This : in out Extract_Line; Value : Boolean) is
   begin
      This.Coloration := Value;
   end Set_Coloration;

   --------------------
   -- Get_Coloration --
   --------------------

   function Get_Coloration (This : Extract_Line) return Boolean is
   begin
      return This.Coloration;
   end Get_Coloration;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This, Prev : Ptr_Extract_Line; Container : in out Extract) is
   begin
      if Prev = null then
         Container.First := This.Next;
      else
         Prev.Next := This.Next;
      end if;
      This.Next := null;
   end Remove;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (This : Ptr_Extract_Line; Cursor : File_Cursor'Class)
     return Ptr_Extract_Line is
   begin
      if This = null then
         return null;
      elsif This.Cursor = File_Cursor (Cursor) then
         return This;
      elsif This.Next = null then
         return null;
      else
         return Get_Line (This.Next, Cursor);
      end if;
   end Get_Line;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   ----------------------
   -- Unchecked_Assign --
   ----------------------

   procedure Unchecked_Assign (This, Value : in out Extract'Class) is
   begin
      This.First := Value.First;
      This.Caption := Value.Caption;
   end Unchecked_Assign;

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (This : in out Extract) is
   begin
      This.First := null;
      This.Caption := null;
   end Unchecked_Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract) return Extract is
      New_Extract : Extract := This;
   begin
      if New_Extract.First /= null then
         New_Extract.First :=
           new Extract_Line'(Clone (New_Extract.First.all, True));
      end if;
      New_Extract.Caption := Clone (New_Extract.Caption);

      return New_Extract;
   end Clone;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract) is
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Line,
            Cursor          => File_Cursor (Clone (Cursor)),
            Original_Length => Len,
            Content         => new String'(Get (This, Cursor, Len)),
            Next            => null,
            Coloration      => True));
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract)
   is
      Str : constant String := Get_Line (This, Cursor);
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Line,
            Cursor          => Clone (Cursor),
            Original_Length => Str'Length,
            Content         => new String'(Str),
            Next            => null,
            Coloration      => True));
   end Get_Line;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This     : Extract;
      Position : Natural := 1) return String
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Position - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      return Current_Extract.Content.all;
   end Get_String;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String
     (This     : Extract;
      Value    : String;
      Position : Natural := 1)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Position - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      Assign (Current_Extract.Content, Value);
      Current_Extract.Context := Line_Modified;
   end Set_String;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         Commit (Current_Extract.all, Current_Text, Offset_Line);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract) is
   begin
      if This.First /= null then
         Free (This.First.all);
         Free (This.First);
      end if;

      Free (This.Caption);
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Extract'Class) is
   begin
      Free (This);
   end Free_Data;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String is

      Extended_Extract : Extract;

   begin

      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);

      declare
         Buffer : String := Get_New_Text (Extended_Extract);
      begin
         Free (Extended_Extract);
         return Buffer;
      end;

   end Get_New_Text;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text (This : Extract) return String is
      Current_Extract  : Ptr_Extract_Line := This.First;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
      Buffer : String
        (1 .. Get_New_Text_Length (This) +
           EOL_Str'Length * Get_Number_Lines (This));
   begin

      Current_Extract := This.First;
      while Current_Extract /= null loop
         Current_Length := Get_New_Text_Length (Current_Extract.all)
           + EOL_Str'Length;
         Buffer (Current_Col ..
                   Current_Col + Current_Length - 1) :=
             Get_New_Text (Current_Extract.all) & EOL_Str;
         Current_Col := Current_Col + Current_Length;
         Current_Extract := Current_Extract.Next;
      end loop;

      return Buffer;

   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String
   is
      Current_Extract  : Ptr_Extract_Line;
      Extended_Extract : Extract;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
   begin

      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);
      Current_Extract := Extended_Extract.First;

      declare
         Buffer : String (1 .. Get_Old_Text_Length
                            (Extended_Extract,
                             Current_Text));
      begin

         while Current_Extract /= null loop
            Current_Length := Get_Old_Text_Length
              (Current_Extract.all,
            Current_Text);
            Buffer (Current_Col ..
                   Current_Col + Current_Length - 1) :=
              Get_Old_Text (Current_Extract.all, Current_Text);
            Current_Col := Current_Col + Current_Length;
            Current_Extract := Current_Extract.Next;
         end loop;

         Free (Extended_Extract);

         return Buffer;
      end;

   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length (This : Extract) return Natural is
   begin
      if This.First /= null then
         return Get_New_Text_Length (This.First.all, True);
      else
         return 0;
      end if;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class) return Natural is
   begin
      if This.First /= null then
         return Get_Old_Text_Length (This.First.all, Current_Text, True);
      else
         return 0;
      end if;
   end Get_Old_Text_Length;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : Extract; Position : File_Cursor'Class) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         if Current_Extract.Cursor.Line = Position.Line
           and Current_Extract.Cursor.File_Name.all = Position.File_Name.all
         then
            return Current_Extract;
         end if;

         Current_Extract := Current_Extract.Next;
      end loop;

      Raise_Exception
        (Text_Manager_Error'Identity,
         "line" & Natural'Image (Position.Line) & " of " &
           Position.File_Name.all & " not found in an extract");
   end Get_Line;

   ----------------
   -- Get_Record --
   ----------------

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
      Iterator        : Integer;

   begin

      Iterator := 1;
      loop
         if Current_Extract = null then
            Raise_Exception
              (Text_Manager_Error'Identity,
               "record" & Natural'Image (Number) & " not found in an extract");
         end if;


         exit when Iterator = Number;

         Iterator := Iterator + 1;
         Current_Extract := Current_Extract.Next;

      end loop;

      return Current_Extract;
   end Get_Record;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_String   : String;
      Format_Old   : String_Mode := Text_Ascii)
   is
      Word_Length  : Natural;
      Old_Line     : Dynamic_String;
      Current_Line : Ptr_Extract_Line;

   begin
      Current_Line := Get_Line (This, Cursor);
      Assign (Old_Line, Current_Line.Content);

      case Format_Old is
         when Regular_Expression =>
            Word_Length := Get_Word_Length
              (Current_Line.all, Cursor.Col, Old_String);

         when Text_Ascii =>
            Word_Length := Old_String'Length;

      end case;

      Assign
        (Current_Line.Content,
         Old_Line (1 .. Cursor.Col - 1) & New_String & Old_Line
           (Cursor.Col + Word_Length .. Old_Line'Last));

      Free (Old_Line);

      Current_Line.Context := Line_Modified;
   end Replace_Word;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_Length   : Natural) is

      Old_Line     : Dynamic_String;
      Current_Line : Ptr_Extract_Line;

   begin
      Current_Line := Get_Line (This, Cursor);
      Assign (Old_Line, Current_Line.Content);

      Assign
        (Current_Line.Content,
         Old_Line (1 .. Cursor.Col - 1) & New_String & Old_Line
           (Cursor.Col + Old_Length .. Old_Line'Last));

      Free (Old_Line);

      Current_Line.Context := Line_Modified;
   end Replace_Word;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Word   : String)
   is
      Current_Line : Ptr_Extract_Line := Get_Line (This, Cursor);
      Old_String   : Dynamic_String;
   begin
      Assign (Old_String, Current_Line.Content);
      Assign (Current_Line.Content, Old_String (1 .. Cursor.Col - 1) &
              Word &
                Old_String (Cursor.Col .. Old_String.all'Last));

      Current_Line.Context := Line_Modified;

      Free (Old_String);
   end Add_Word;

   ----------------------
   -- Get_Number_Lines --
   ----------------------

   function Get_Number_Lines (This : Extract) return Natural is
   begin
      if This.First = null then
         return 0;
      else
         return Length (This.First.all);
      end if;
   end Get_Number_Lines;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String) is

      Line_Cursor : File_Cursor := File_Cursor (Clone (Cursor));

   begin
      Line_Cursor.Col := 1;
      Add_Element
        (This, new Extract_Line'
           (Context         => Line_Created,
            Cursor          => Line_Cursor,
            Original_Length => 0,
            Content         => new String'(Text),
            Next            => null,
            Coloration      => True));
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class)
   is
      Line : Ptr_Extract_Line;
   begin
      Line := Get_Line (This, Cursor);
      Line.Context := Line_Deleted;
   end Delete_Line;

   ----------------------
   -- Delete_All_Lines --
   ----------------------

   procedure Delete_All_Lines (This : in out Extract) is
      Line : Ptr_Extract_Line := This.First;
   begin
      while Line /= null loop
         Line.Context := Line_Deleted;
         Line := Line.Next;
      end loop;
   end Delete_All_Lines;

   -----------------
   -- Add_Element --
   -----------------

   --  Assertion : This and Previous are not null together
   procedure Add_Element
     (This, Previous, Element : Ptr_Extract_Line;
      Container : in out Extract) is
   begin
      if This = null then
         Previous.Next := Element;
      else
         if This.Cursor.Line > Element.Cursor.Line then
            Element.Next := This;
            if Previous /= null then
               Previous.Next := Element;
            else
               Container.First := Element;
            end if;
         else
            Add_Element (This.Next, This, Element, Container);
         end if;
      end if;
   end Add_Element;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line) is
   begin
      if This.First = null then
         This.First := Element;
      else
         Add_Element (This.First, null, Element, This);
      end if;
   end Add_Element;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract;
      Cursor : File_Cursor'Class;
      Format : String) return Natural is
   begin
      return Get_Word_Length
        (Get_Line (This, Cursor).all,
         Cursor.Col,
         Format);
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This         : Extract;
      Searched     : String;
      Cursor       : File_Cursor'Class := Null_File_Cursor;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True)
     return File_Cursor'Class is

      Current                 : Ptr_Extract_Line;
      Result, Current_Cursor  : File_Cursor := Null_File_Cursor;

   begin

      if File_Cursor (Cursor) /= Null_File_Cursor then
         Current_Cursor := File_Cursor (Cursor);
      else
         if Step = Normal_Step then
            Current_Cursor :=
              File_Cursor (Get_Cursor (Get_First_Line (This).all));
         else
            null; --  ??? Soon programmed
         end if;
      end if;

      Current := Get_Line (This, Current_Cursor);

      loop

         if Current = null then return Null_File_Cursor; end if;

         Result := File_Cursor (Search_String
                                  (Current.all,
                                   Current_Cursor,
                                   Searched,
                                   Step,
                                   Jump_String));

         if Result /= Null_File_Cursor then
            Result.Line := Get_Cursor (Current.all).Line;
            return Result;
         end if;

         exit when Current = null;

         case Step is
            when Normal_Step =>
               Current := Current.Next;
               Current_Cursor.Col := 1;
            when Reverse_Step =>
               Current := Previous (This, Current);
               Current_Cursor.Col := 0;
         end case;

      end loop;

      return Null_File_Cursor;

   end Search_String;

   --------------
   -- Previous --
   --------------

   function Previous (Container : Extract; Node : Ptr_Extract_Line)
     return Ptr_Extract_Line is

      Current : Ptr_Extract_Line := Container.First;

   begin
      if Current = Node then return null; end if;

      while Current.Next /= Node loop
         Current := Current.Next;
         if Current = null then
            Raise_Exception
              (Codefix_Panic'Identity,
               "Line unknowm in the specified extract");
         end if;
      end loop;
      return Current;
   end Previous;

   -----------------
   -- Set_Caption --
   -----------------

   procedure Set_Caption (This : in out Extract; Caption : String) is
   begin
      Assign (This.Caption, Caption);
   end Set_Caption;

   -----------------
   -- Get_Caption --
   -----------------

   function Get_Caption (This : Extract) return String is
   begin
      if This.Caption /= null then
         return This.Caption.all;
      else
         return "Uncaptionned correction";
      end if;
   end Get_Caption;

   --------------------
   -- Get_First_Line --
   --------------------

   function Get_First_Line (This : Extract) return Ptr_Extract_Line is
   begin
      return This.First;
   end Get_First_Line;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural) is

      Null_Prev : Ptr_Extract_Line := null;

   begin
      Extend_Before (This.First, Null_Prev, Current_Text, Size);
   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural) is
   begin
      Extend_After (This.First, Current_Text, Size);
   end Extend_After;

   ------------
   -- Reduce --
   ------------

   procedure Reduce
     (This                    : in out Extract;
      Size_Before, Size_After : Natural) is

      type Lines_Array is array (Natural range <>) of Ptr_Extract_Line;

      procedure Delete_First;
      procedure Add_Last;

      Current_Line : Ptr_Extract_Line;
      Lines        : Lines_Array (1 .. Size_Before);
      Left_After   : Natural := 0;
      Next_Line    : Ptr_Extract_Line;
      Prev_Line    : Ptr_Extract_Line;

      procedure Delete_First is
      begin
         Remove (Lines (1), Previous (This, Lines (1)), This);
         Free (Lines (1).all);
         Free (Lines (1));
         Lines (1 .. Lines'Last - 1) := Lines (2 .. Lines'Last);
         Lines (Lines'Last) := null;
      end Delete_First;

      procedure Add_Last is
      begin
         for J in Lines'Range loop
            if Lines (J) = null then
               Lines (J) := Current_Line;
               return;
            end if;
         end loop;

         Delete_First;
         Lines (Lines'Last) := Current_Line;
      end Add_Last;

   begin

      Current_Line := Get_First_Line (This);

      while Current_Line /= null loop
         Next_Line := Next (Current_Line.all);

         if Current_Line.Coloration = False
           or else Current_Line.Context = Original_Line
         then
            if Left_After = 0 then
               if Size_Before = 0 then
                  Remove (Current_Line, Prev_Line, This);
                  Free (Current_Line.all);
                  Free (Current_Line);
               else
                  Prev_Line := Current_Line;
                  Add_Last;
               end if;
            else
               Prev_Line := Current_Line;
               Left_After := Left_After + 1;
            end if;
         else
            Prev_Line := Current_Line;
            Left_After := Size_After;
            Lines := (others => null);
         end if;

         Current_Line := Next_Line;
      end loop;

      if Left_After = 0 then
         for J in Lines'Range loop
            Remove (Lines (J), Previous (This, Lines (J)), This);
            Free (Lines (J).all);
            Free (Lines (J));
         end loop;
      end if;

   end Reduce;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class)
   is
      Current_Line : Ptr_Extract_Line;
      Line_Cursor  : File_Cursor := File_Cursor (Start);
   begin
      Line_Cursor.Col := 1;

      if Start.Line = Stop.Line then
         Current_Line := Get_Line (This, Line_Cursor);
         Set_String
           (Current_Line.all,
            Current_Line.Content
              (Current_Line.Content'First .. Start.Col - 1) &
              Current_Line.Content (Stop.Col + 1 ..
                                      Current_Line.Content'Last));
      else
         Current_Line := Get_Line (This, Line_Cursor);
         Set_String
           (Current_Line.all,
            Current_Line.Content
              (Current_Line.Content'First .. Start.Col - 1));

         for J in Start.Line + 1 .. Stop.Line - 1 loop
            Current_Line := Next (Current_Line.all);
            Current_Line.Context := Line_Deleted;
         end loop;
         Current_Line := Next (Current_Line.all);

         Set_String
           (Current_Line.all,
            Current_Line.Content (Stop.Col + 1 .. Current_Line.Content'Last));
      end if;
   end Erase;


   ----------------------------------------------------------------------------
   --  Merge functions and uilities
   ----------------------------------------------------------------------------

   type Char_Context is (Original, Added, Modified, Removed);

   type Merge_Array is array (Positive range <>) of Char_Context;

   procedure Merge_Tree
     (Original_Str, New_Str          : String;
      Original_It, New_It, Result_It : Positive;
      Offset_Char                    : Integer;
      Result                         : out Merge_Array);

   function Get_Merge_Array (Original_Str, New_Str : String)
     return Merge_Array;

   ---------------------
   -- Get_Merge_Array --
   ---------------------

   function Get_Merge_Array (Original_Str, New_Str : String)
     return Merge_Array is

      Offset       : Integer;
      Array_Length : Natural;

   begin

      Offset := New_Str'Length - Original_Str'Length;

      if Offset > 0 then
         Array_Length := New_Str'Length;
      else
         Array_Length := Original_Str'Length;
      end if;

      declare
         Result : Merge_Array (1 .. Array_Length);
      begin
         Merge_Tree
           (Original_Str,
            New_Str,
            Original_Str'First,
            New_Str'First,
            1,
            Offset,
            Result);
         return Result;
      end;

   end Get_Merge_Array;

   ----------------
   -- Merge_Tree --
   ----------------

   procedure Merge_Tree
     (Original_Str, New_Str           : String;
      Original_It, New_It, Result_It  : Positive;
      Offset_Char                     : Integer;
      Result                          : out Merge_Array) is
   begin

      if Result_It > Result'Last then
         return;
      end if;

      if Original_It <= Original_Str'Last
        and then New_It <= New_Str'Last
        and then Original_Str (Original_It) = New_Str (New_It)
      then
         Result (Result_It) := Original;
         Merge_Tree
           (Original_Str,
            New_Str,
            Original_It + 1,
            New_It + 1,
            Result_It + 1,
            Offset_Char,
            Result);
      else
         if Offset_Char > 0 then
            Result (Result_It) := Added;
            Merge_Tree
              (Original_Str,
               New_Str,
               Original_It,
               New_It + 1,
               Result_It + 1,
               Offset_Char - 1,
               Result);
         elsif Offset_Char < 0 then
            Result (Result_It) := Removed;
            Merge_Tree
              (Original_Str,
               New_Str,
               Original_It + 1,
               New_It,
               Result_It + 1,
               Offset_Char + 1,
               Result);
         else
            Result (Result_It) := Modified;
            Merge_Tree
              (Original_Str,
               New_Str,
               Original_It + 1,
               New_It + 1,
               Result_It + 1,
               Offset_Char,
               Result);
         end if;
      end if;

   end Merge_Tree;

   -----------
   -- Merge --
   -----------


   procedure Merge
     (New_Str : out Dynamic_String;
      Original_Str, Str_1, Str_2 : String;
      Success : out Boolean) is

      procedure Loop_Add (Num : Integer);
      procedure Merge_1_Original;
      procedure Merge_1_Modified;
      procedure Merge_1_Removed;
      procedure Merge_1_Added;

      Merge_1                : constant Merge_Array :=
        Get_Merge_Array (Original_Str, Str_1);
      Merge_2                : constant Merge_Array :=
        Get_Merge_Array (Original_Str, Str_2);
      It_Merge_1, It_Merge_2 : Integer := 1;
      It_Str_1, It_Str_2     : Integer;
      Result                 : Unbounded_String;

      procedure Loop_Add (Num : Integer) is
      begin
         case Num is
            when 1 =>
               while Merge_1 (It_Merge_1) = Added loop
                  Result := Result & Str_1 (It_Str_1);
                  It_Str_1 := It_Str_1 + 1;
                  It_Merge_1 := It_Merge_1 + 1;
               end loop;
            when 2 =>
               while Merge_2 (It_Merge_2) = Added loop
                  Result := Result & Str_2 (It_Str_2);
                  It_Str_2 := It_Str_2 + 1;
                  It_Merge_2 := It_Merge_2 + 1;
               end loop;
            when others =>
               null;
         end case;
      end Loop_Add;

      procedure Merge_1_Original is
      begin
         case Merge_2 (It_Merge_2) is
            when Original =>
               Result := Result & Str_1 (It_Str_1);
               It_Str_1 := It_Str_1 + 1;
               It_Str_2 := It_Str_2 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Modified =>
               Result := Result & Str_2 (It_Str_2);
               It_Str_1 := It_Str_1 + 1;
               It_Str_2 := It_Str_2 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Removed =>
               It_Str_1 := It_Str_1 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Added =>
               Loop_Add (2);
         end case;
      end Merge_1_Original;

      procedure Merge_1_Modified is
      begin
         case Merge_2 (It_Merge_2) is
            when Original =>
               Result := Result & Str_2 (It_Str_2);
               It_Str_1 := It_Str_1 + 1;
               It_Str_2 := It_Str_2 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Modified =>
               if Str_1 (It_Str_1) /= Str_2 (It_Str_2) then
                  Success := False;
                  return;
               end if;
               Result := Result & Str_2 (It_Str_2);
               It_Str_1 := It_Str_1 + 1;
               It_Str_2 := It_Str_2 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Removed =>
               Success := False;
            when Added =>
               Loop_Add (2);
         end case;
      end Merge_1_Modified;

      procedure Merge_1_Removed is
      begin
         case Merge_2 (It_Merge_2) is
            when Original =>
               It_Str_2 := It_Str_2 + 1;
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Modified =>
               Success := False;
            when Removed =>
               It_Merge_1 := It_Merge_1 + 1;
               It_Merge_2 := It_Merge_2 + 1;
            when Added =>
               Loop_Add (2);
         end case;
      end Merge_1_Removed;

      procedure Merge_1_Added is
      begin
         case Merge_2 (It_Merge_2) is
            when Original =>
               Loop_Add (1);
            when Modified =>
               Loop_Add (1);
            when Removed =>
               Loop_Add (1);
            when Added =>
               Loop_Add (1);
               Loop_Add (2);
         end case;
      end Merge_1_Added;

   begin
      Success := True;

      It_Str_1 := Str_1'First;
      It_Str_2 := Str_2'First;

      loop
         case Merge_1 (It_Merge_1) is
            when Original =>
               Merge_1_Original;
            when Modified =>
               Merge_1_Modified;
            when Removed =>
               Merge_1_Removed;
            when Added =>
               Merge_1_Added;
         end case;

         if Success = False then
            return;
         end if;

         exit when Merge_1'Last <  It_Merge_1;
      end loop;

      if It_Merge_2 <= Merge_2'Last then
         if Merge_2 (It_Merge_2) = Added then
            Loop_Add (2);
         else
            Raise_Exception
              (Codefix_Panic'Identity,
               "Merge_2 (It_Merge_2) supposed to be Added");
         end if;
      end if;

      Assign (New_Str, To_String (Result));

   end Merge;

   -----------
   -- Merge --
   -----------

   type Virtual_Navigator is new Text_Navigator_Abstr with null record;

   function New_Text_Interface (This : Virtual_Navigator) return Ptr_Text;

   function New_Text_Interface (This : Virtual_Navigator) return Ptr_Text is
      pragma Unreferenced (This);
   begin
      return null;
   end New_Text_Interface;


   procedure Merge
     (This                 : out Extract;
      Extract_1, Extract_2 : Extract'Class;
      Success              : out Boolean)
   is
      Navigator : Virtual_Navigator;
   begin
      Merge
        (This,
         Extract_1, Extract_2,
         Navigator,
         Success,
         False);
      Free (Navigator);
   end Merge;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (This                 : out Extract;
      Extract_1, Extract_2 : Extract'Class;
      Current_Text         : Text_Navigator_Abstr'Class;
      Success              : out Boolean;
      Merge_Characters     : Boolean := True) is

      Line_1, Line_2, New_Line : Ptr_Extract_Line;

      procedure Line_1_Original;
      procedure Line_1_Modified;
      procedure Line_1_Deleted;
      procedure Line_1_Created;

      procedure Line_1_Original is
      begin
         case Line_2.Context is
            when Original_Line =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Modified =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Deleted =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Created =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
         end case;
      end Line_1_Original;

      procedure Line_1_Modified is
      begin
         case Line_2.Context is
            when Original_Line =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Modified =>
               if Merge_Characters then
                  New_Line := new Extract_Line'(Clone (Line_1.all, False));
                  Merge
                    (New_Line.Content,
                     Get_Old_Text (Line_1.all, Current_Text),
                     Line_1.Content.all,
                     Line_2.Content.all,
                     Success);

                  if Is_Blank (New_Line.Content.all) then
                     New_Line.Context := Line_Deleted;
                  end if;

                  Add_Element (This, New_Line);
               else
                  Add_Element (This, new Extract_Line'(Clone
                                                         (Line_2.all, False)));
               end if;
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Deleted =>
               Success := False;
            when Line_Created =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
         end case;
      end Line_1_Modified;

      procedure Line_1_Deleted is
      begin
         case Line_2.Context is
            when Original_Line =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Modified =>
               Success := False;
            when Line_Deleted =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
               Line_2 := Next (Line_2.all);
            when Line_Created =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               Line_1 := Next (Line_1.all);
         end case;
      end Line_1_Deleted;

      procedure Line_1_Created is
      begin
         case Line_2.Context is
            when Original_Line =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
               Line_2 := Next (Line_2.all);
            when Line_Modified =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
               Line_2 := Next (Line_2.all);
            when Line_Deleted =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
               Line_2 := Next (Line_2.all);
            when Line_Created =>
               Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
               if Line_1.all /= Line_2.all then
                  Line_1 := Next (Line_1.all);
               else
                  Line_1 := Next (Line_1.all);
                  Line_2 := Next (Line_2.all);
               end if;
         end case;
      end Line_1_Created;


   begin
      Line_1 := Get_First_Line (Extract_1);
      Line_2 := Get_First_Line (Extract_2);
      Success := True;

      while Line_1 /= null and then Line_2 /= null loop
         if Line_1.Cursor < Line_2.Cursor then
            Add_Element (This, new Extract_Line'(Clone (Line_1.all, False)));
            Line_1 := Next (Line_1.all);
         elsif Line_2.Cursor < Line_1.Cursor then
            Add_Element (This, new Extract_Line'(Clone (Line_2.all, False)));
            Line_2 := Next (Line_2.all);
         else

            case Line_1.Context is
               when Original_Line =>
                  Line_1_Original;
               when Line_Modified =>
                  Line_1_Modified;
               when Line_Created =>
                  Line_1_Created;
               when Line_Deleted =>
                  Line_1_Deleted;
            end case;
         end if;

         if not Success then
            return;
         end if;
      end loop;

      if Line_1 /= null then
         while Line_1 /= null loop
            Add_Element (This, new Extract_Line'(Clone
                                                      (Line_1.all, False)));
            Line_1 := Next (Line_1.all);
         end loop;
      elsif Line_2 /= null then
         while Line_2 /= null loop
            Add_Element (This, new Extract_Line'(Clone
                                                      (Line_2.all, False)));
            Line_2 := Next (Line_2.all);
         end loop;
      end if;

   end Merge;

end Codefix.Text_Manager;
