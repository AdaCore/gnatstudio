with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat; use GNAT.Regpat;

package body Text_Manager is

   ----------------------------------------------------------------------------
   --  type Dynamic_String
   ----------------------------------------------------------------------------

   ------------
   -- Affect --
   ------------

   procedure Affect (This : in out Dynamic_String; Value : String) is
   begin
      Free (This);
      This := new String'(Value);
   end Affect;

   ------------
   -- Affect --
   ------------

   procedure Affect (This : in out Dynamic_String; Value : Dynamic_String) is
   begin
      Free (This);
      This := new String'(Value.all);
   end Affect;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (This : in out Dynamic_String) is
   begin
      Get_Line (Standard_Input, This);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line (File : File_Type; This : in out Dynamic_String) is
      Len    : Natural;
      Buffer : String (1 .. 256);
   begin
      Get_Line (File, Buffer, Len);
      Free (This);
      This := new String'(Buffer (1 .. Len));
   end Get_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (This : Dynamic_String) is
   begin
      Put_Line (Standard_Output, This);
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : File_Type; This : Dynamic_String) is
   begin
      Put_Line (File, This.all);
   end Put_Line;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   procedure Free (This : in out Ptr_Text) is
      procedure Delete is new
         Ada.Unchecked_Deallocation (Text_Interface'Class, Ptr_Text);
   begin
      Delete (This);
   end Free;

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
      Current_Text : in out Text_Interface'Class;
      Offset_Line  : in out Natural) is

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
      Recursive : Boolean := True)
      return Extract_Line is
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

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : File_Cursor'Class)
     return Natural is
   begin
      return Get_Line (This, Cursor)'Length;
   end Line_Length;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract) is
   begin
      Add_Element (Destination,
                   new Extract_Line'(Original_Line,
                                     File_Cursor (Clone (Cursor)),
                                     Len,
                                     new String'(Get (This, Cursor, Len)),
                                                 null));
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract) is

      Str : String := Get_Line (This, Cursor);

   begin
      Add_Element (Destination, new Extract_Line'(Original_Line,
                                                  File_Cursor (Clone (Cursor)),
                                                  Str'Length,
                                                  new String'(Str),
                                                  null));
   end Get_Line;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This : Extract;
      Position : Natural := 1)
     return String is

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
      Position : Natural := 1) is

      Current_Extract : Ptr_Extract_Line := This.First;

   begin
      for J in 1 .. Position - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      Affect (Current_Extract.Content, Value);
   end Set_String;

   ------------
   -- Update --
   ------------

   procedure Update
     (This         : Extract;
      Current_Text : in out Text_Interface'Class;
      Offset_Line  : in out Natural) is

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
     (This : Extract;
      Current_Text : Text_Interface'Class) is
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

   function Get_Line (This : Extract; Number : Natural)
      return Ptr_Extract_Line is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         if Current_Extract.Cursor.Line = Number then
            return Current_Extract;
         end if;
         Current_Extract := Current_Extract.Next;
      end loop;

      Raise_Exception (Text_Manager_Error'Identity, "line" &
                       Natural'Image (Number) &
                       " not found in an extract");
   end Get_Line;


   ----------------
   -- Get_Record --
   ----------------

   --  C pas beau la levee de l'exception. A changer.
   function Get_Record (This : Extract; Number : Natural)
     return Ptr_Extract_Line is
         Current_Extract : Ptr_Extract_Line := This.First;
   begin
      if Current_Extract = null then
         Raise_Exception (Text_Manager_Error'Identity, "record" &
                          Natural'Image (Number) &
                          " not found in an extract");
      end if;
      for I in 1 .. Number - 1 loop
         Current_Extract := Current_Extract.Next;
         if Current_Extract = null then
            Raise_Exception (Text_Manager_Error'Identity, "record" &
                             Natural'Image (Number) &
                             " not found in an extract");
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

   procedure Put_Line (This : Extract_Line) is
   begin
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
     (This : Extract_Line;
      Current_Text : Text_Interface'Class) is

      Old_Extract : Extract;

   begin
      case This.Context is
         when Original_Line | Line_Modified =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);
            Put_Line (Old_Extract);
            Free (Old_Extract);
         when Line_Created =>
            null;
         when Line_Deleted =>
            null;
      end case;


   end Put_Line_Original;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : Text_Cursor'Class;
      New_String   : String;
      Format       : String := "(^[\w]*)") is

      Word_Length  : Natural;
      Old_String   : Dynamic_String;
      Current_Line : Ptr_Extract_Line;

   begin
      Current_Line := Get_Line (This, Cursor.Line);
      Affect (Old_String, Current_Line.Content);
      Word_Length := Get_Word_Length (Current_Line.all, Cursor.Col, Format);

      Affect (Current_Line.Content, Old_String (1 .. Cursor.Col - 1) &
              New_String &
              Old_String (Cursor.Col + Word_Length .. Old_String'Length));

      Free (Old_String);
   end Replace_Word;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word
     (This   : in out Extract;
      Cursor : Text_Cursor'Class;
      Word   : String) is

      Current_Line : Ptr_Extract_Line := Get_Line (This, Cursor.Line);
      Old_String   : Dynamic_String;

   begin
      Affect (Old_String, Current_Line.Content);
      Affect (Current_Line.Content, Old_String (1 .. Cursor.Col - 1) &
              Word &
              Old_String (Cursor.Col .. Old_String.all'Length));
      Free (Old_String);
   end Add_Word;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract;
      Cursor : Text_Cursor'Class;
      Format : String)
     return Natural is
   begin
      return Get_Word_Length
        (Get_Line (This, Cursor.Line).all,
         Cursor.Col,
         Format);
   end Get_Word_Length;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Natural;
      Format : String)
      return Natural is

      Matches : Match_Array (1 .. 1);
      Matcher  : constant Pattern_Matcher := Compile (Format);
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
   -- New_Line --
   --------------

   procedure New_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String) is
   begin
      Add_Element (This, new Extract_Line'
                   (Line_Created,
                    Clone (Cursor),
                    0,
                    new String'(Text),
                    null));
   end New_Line;

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

end Text_Manager;
