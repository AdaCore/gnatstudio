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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;
with Basic_Types; use Basic_Types;
with String_Utils; use String_Utils;

package body Codefix.Text_Manager is

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   function "<" (Left, Right : Text_Cursor) return Boolean is
   begin
      return Left.Line < Right.Line or else Left.Col < Right.Col;
   end "<";

   function ">" (Left, Right : Text_Cursor) return Boolean is
   begin
      return not (Left < Right) and then Left /= Right;
   end ">";


   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   procedure Free (This : in out Text_Navigator_Abstr) is
   begin
      Free (This.Files.all);
      Free (This.Files);
   end Free;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Current_Text : Text_Navigator_Abstr;
      Cursor       : File_Cursor'Class) return Construct_Information is
   begin
      return Get_Unit
        (Get_File (Current_Text, Cursor.File_Name.all).all,
         Text_Cursor (Cursor));
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
      Indent      : Natural;
      Next_Indent : Natural;
      Buffer      : Dynamic_String;

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
         Current_Indent => Next_Indent,
         Prev_Indent => Indent,
         Callback => null);

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
      Cursor : File_Cursor)
   is
      Unit_Info, Body_Info : Construct_Information;
      Line_Cursor          : File_Cursor := Cursor;
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

   procedure Update (This : Text_Navigator_Abstr) is
      Iterator : Text_List.List_Node := First (This.Files.all);
   begin
      while Iterator /= Text_List.Null_Node loop
         Update (Data (Iterator).all);
         Iterator := Next (Iterator);
      end loop;
   end Update;

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
     (Current_Text : Text_Interface;
      Cursor       : Text_Cursor'Class) return Construct_Information
   is
      Current_Info : Construct_Access;
   begin
      Current_Info := Current_Text.Tokens_List.First;

      while Current_Info /= null loop
         if (Current_Info.Sloc_Start.Line = Cursor.Line
             and then Current_Info.Sloc_Start.Column = Cursor.Col)
           or else (Current_Info.Sloc_Entity.Line = Cursor.Line
                    and then Current_Info.Sloc_Entity.Column = Cursor.Col)
         then
            return Current_Info.all;
         end if;

         Current_Info := Current_Info.Next;
      end loop;

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
      function Normalize (Str : String_Access) return String;
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

      function Normalize (Str : String_Access) return String is
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
            --  Test de fin d'encapsulation
            if Current_Info.Sloc_End.Line < Stop.Line
              or else (Current_Info.Sloc_End.Line = Stop.Line
                       and then Current_Info.Sloc_End.Column < Stop.Column)
            then
               return;
            end if;

            if not Current_Info.Is_Declaration and then --  test id body
              Current_Info.Name.all = Spec.Name.all and then
              Normalize (Current_Info.Profile) = Normalize (Spec.Profile)
            then
               Result := Current_Info;
            end if;

            --  Test de debut d'encapsulation

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

            elsif Current_Info.Is_Declaration and then  --  test id spec
              Current_Info.Name.all = Spec.Name.all and then
              Current_Info.Sloc_Start = Spec.Sloc_Start and then
              Normalize (Current_Info.Profile) = Normalize (Spec.Profile)
            then
               Found := True;
               return;
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


   function Search_String
     (This         : Text_Interface'Class;
      Cursor       : File_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True)
     return File_Cursor'Class is
   begin
      --  Soon programmed
      return Null_File_Cursor;
   end Search_String;
   --  Search a string in the text and returns a cursor at the beginning. If
   --  noting is found, then the cursor is Null_Cursor.

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
      New_Cursor.File_Name := new String'(This.File_Name.all);
      return New_Cursor;
   end Clone;

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

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

   procedure Update
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
   end Update;

   -----------
   -- Clone --
   -----------

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean := True) return Extract_Line
   is
      New_Line : Extract_Line := This;
   begin
      New_Line.Cursor := Clone (New_Line.Cursor);
      New_Line.Content := new String'(New_Line.Content.all);

      if Recursive and then New_Line.Next /= null then
         New_Line.Next := new Extract_Line'(Clone (New_Line.Next.all));
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
      Str_Parsed : String :=  This.Content.all;

   begin
      Match (Matcher, Str_Parsed (Col .. Str_Parsed'Length), Matches);

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
      Jump_String  : Boolean := True)
   return File_Cursor'Class is

      Result : File_Cursor := File_Cursor (Cursor);

   begin
      if Result.Col = 0 then Result.Col := This.Content.all'Length; end if;

      case Step is
         when Normal_Step =>
            for J in Result.Col .. This.Content.all'Length -
              Searched'Length + 1
            loop
               if This.Content.all (J .. J + Searched'Length - 1) =
                 Searched
               then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;
         when Reverse_Step =>
            for J in reverse 1 .. Result.Col - Searched'Length + 1 loop
               if This.Content.all (J .. J + Searched'Length - 1) =
                 Searched
               then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;
      end case;

      return Null_File_Cursor;

   end Search_String;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract) return Extract is
      New_Extract : Extract := This;
   begin
      if New_Extract.First /= null then
         New_Extract.First := new Extract_Line'(Clone (New_Extract.First.all));
      end if;

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
           (Original_Line,
            File_Cursor (Clone (Cursor)),
            Len,
            new String' (Get (This, Cursor, Len)), null));
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
           (Original_Line,
            File_Cursor (Clone (Cursor)),
            Str'Length,
            new String'(Str),
            null));
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
   end Set_String;

   ------------
   -- Update --
   ------------

   procedure Update
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Natural)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         Update (Current_Extract.all, Current_Text, Offset_Line);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Update;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract) is
   begin
      Free (This.First.all);
      Free (This.First);
   end Free;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : Extract) is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         Put_Line (Current_Extract.all);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Put_Line;

   -----------------------
   -- Put_Line_Original --
   -----------------------

   procedure Put_Line_Original
     (This          : Extract;
      Current_Text : Text_Navigator_Abstr'Class)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         Put_Line_Original (Current_Extract.all, Current_Text);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Put_Line_Original;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : Extract; Position : File_Cursor) return Ptr_Extract_Line
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
         "line" & Natural'Image (Position.Line) & "of " &
           Position.File_Name.all & " not found in an extract");
   end Get_Line;

   ----------------
   -- Get_Record --
   ----------------

   --  ??? C pas beau la levee de l'exception. A changer.

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      if Current_Extract = null then
         Raise_Exception
           (Text_Manager_Error'Identity, "record" &
              Natural'Image (Number) & " not found in an extract");
      end if;

      for J in 1 .. Number - 1 loop
         Current_Extract := Current_Extract.Next;

         if Current_Extract = null then
            Raise_Exception
              (Text_Manager_Error'Identity,
               "record" & Natural'Image (Number) & " not found in an extract");
         end if;
      end loop;

      return Current_Extract;
   end Get_Record;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line) is
   begin
      Destination := (Original_Line, File_Cursor (Cursor), Len,
        new String'(Get (This, Cursor, Len)), null);
   end Get;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : Extract_Line; Detail : Boolean := True) is
   begin
      if Detail then
         case This.Context is
            when Original_Line =>
               Put ("(O)");
            when Line_Modified =>
               Put ("(M)");
            when Line_Created =>
               Put ("(C)");
            when Line_Deleted =>
               Put ("(D)");
         end case;
      else
         Put ("   ");
      end if;

      Put (Natural'Image (This.Cursor.Line));
      Put (":");
      Put (Natural'Image (This.Cursor.Col));
      Put (": " & This.Content.all);
      New_Line;
   end Put_Line;

   -----------------------
   -- Put_Line_Original --
   -----------------------

   procedure Put_Line_Original
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class)
   is
      Old_Extract : Extract;
   begin
      case This.Context is
         when Original_Line | Line_Modified =>
            --  Simplifier l'appel de GET pour n'avoir qu'une ligne et pas
            --  un extrait
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);
            Put_Line (Get_Record (Old_Extract, 1).all, False);
            Free (Old_Extract);

         when Line_Created =>
            null;
         when Line_Deleted =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);
            Put_Line (Get_Record (Old_Extract, 1).all, False);
            Free (Old_Extract);
      end case;
   end Put_Line_Original;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Format       : String := "(^[\w]*)")
   is
      Word_Length  : Natural;
      Old_String   : Dynamic_String;
      Current_Line : Ptr_Extract_Line;

   begin
      Current_Line := Get_Line (This, Cursor);
      Assign (Old_String, Current_Line.Content);
      Word_Length := Get_Word_Length (Current_Line.all, Cursor.Col, Format);

      Assign (Current_Line.Content, Old_String (1 .. Cursor.Col - 1) &
              New_String &
              Old_String (Cursor.Col + Word_Length .. Old_String'Length));

      Free (Old_String);
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
              Old_String (Cursor.Col .. Old_String.all'Length));
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
   begin
      Add_Element
        (This, new Extract_Line'
           (Line_Created, Clone (Cursor), 0, new String'(Text), null));
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
   -- Add_Element --
   -----------------

   --  Assertion : This and Precedent are not null together
   procedure Add_Element
     (This, Precedent, Element : Ptr_Extract_Line;
      Container : in out Extract) is
   begin
      if This = null then
         Precedent.Next := Element;
      else
         if This.Cursor.Line > Element.Cursor.Line then
            Element.Next := This;
            if Precedent /= null then
               Precedent.Next := Element;
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
      Cursor       : File_Cursor'Class;
      Searched     : String;
      Step         : Step_Way := Normal_Step;
      Jump_String  : Boolean := True)
     return File_Cursor'Class is

      Current : Ptr_Extract_Line;
      Result  : File_Cursor := Null_File_Cursor;

   begin

      Current := Get_Line (This, Cursor);

      while Result = Null_File_Cursor and then Current /= null loop
         Result := File_Cursor (Search_String
                                  (Current.all,
                                   Result,
                                   Searched,
                                   Step,
                                   Jump_String));
         case Step is
            when Normal_Step =>
               Current := Current.Next;
               Result.Col := 1;
            when Reverse_Step =>
               Current := Previous (This, Current);
               Result.Col := 0;
         end case;
      end loop;

      return Result;

   end Search_String;

   --------------
   -- Previous --
   --------------

   function Previous (Container : Extract; Node : Ptr_Extract_Line)
     return Ptr_Extract_Line is

      Current : Ptr_Extract_Line := Container.First;

   begin
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

end Codefix.Text_Manager;
