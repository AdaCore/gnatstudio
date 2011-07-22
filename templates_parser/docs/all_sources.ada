with Ada.Text_IO;
with Templates_Parser;

procedure Demo is
   Translations : constant Templates_Parser.Translate_Table :=
     (1 => Templates_Parser.Assoc ("NAME", "Ada"));
begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("demo.tmplt", Translations));
end Demo;

with Ada.Text_IO;
with Templates_Parser;

procedure User1 is

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("USER", True));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("user.tmplt", Translations));
end User1;

with Ada.Text_IO;
with Templates_Parser;

procedure User2 is

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("USER", False));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("user.tmplt", Translations));
end User2;

with Ada.Text_IO;
with Templates_Parser;

procedure Table is

   use type Templates_Parser.Vector_Tag;

   Names : constant Templates_Parser.Vector_Tag
     := +"Bob" & "Bill" & "Toto";
   Ages  : constant Templates_Parser.Vector_Tag
     := +"10" & "30" & "5";

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("NAME", Names),
         2 => Templates_Parser.Assoc ("AGE", Ages));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table.tmplt", Translations));
end Table;

with Ada.Text_IO;
with Templates_Parser;

procedure Table_Inline is

   use type Templates_Parser.Vector_Tag;

   Colors : constant Templates_Parser.Vector_Tag
     := +"Red" & "Green" & "Blue";

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("COLORS", Colors));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_inline.tmplt", Translations));
end Table_Inline;

with Ada.Text_IO;
with Templates_Parser;

procedure If_Inline is

   use type Templates_Parser.Vector_Tag;

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("COND", True));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("if_inline.tmplt", Translations));
end If_Inline;

with Ada.Text_IO;
with Templates_Parser;

procedure Table_Section is

   use type Templates_Parser.Vector_Tag;

   Devices : constant Templates_Parser.Vector_Tag
     := +"Screen" & "Keyboard" & "Mouse" & "Hard Drive";
   Prices  : constant Templates_Parser.Vector_Tag
     := +"$500" & "$20" & "$15" & "$140";

   Translations : constant Templates_Parser.Translate_Table
     := (1 => Templates_Parser.Assoc ("DEVICES", Devices),
         2 => Templates_Parser.Assoc ("PRICES", Prices));

begin
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_section.tmplt", Translations));
end Table_Section;

with Ada.Text_IO;
with Templates_Parser;

procedure Table_If is

   use type Templates_Parser.Vector_Tag;

   function In_Stock (Device : in String) return Boolean;
   --  Complex function. Does a SQL access to the right database to know if
   --  the Device is available and thus can be ordered.

   procedure Add (Device, Price : in String);
   --  Add the device into the list to be displayed

   Devices   : Templates_Parser.Tag;
   Prices    : Templates_Parser.Tag;
   Available : Templates_Parser.Tag;

   ---------
   -- Add --
   ---------

   procedure Add (Device, Price : in String) is
   begin
      Devices   := Devices & Device;
      Prices    := Prices & Price;
      Available := Available & In_Stock (Device);
   end Add;

   --------------
   -- In_Stock --
   --------------

   function In_Stock (Device : in String) return Boolean is
   begin
      if Device = "Keyboard" then
         return True;
      else
         return False;
      end if;
   end In_Stock;

   Translations : Templates_Parser.Translate_Table (1 .. 3);

begin
   Add ("Screen", "$500");
   Add ("Keyboard", "$15");
   Add ("Mouse", "$15");
   Add ("Hard Drive", "$140");

   Translations := (Templates_Parser.Assoc ("DEVICES", Devices),
                    Templates_Parser.Assoc ("PRICES", Prices),
                    Templates_Parser.Assoc ("AVAILABLE", Available));

   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("table_if.tmplt", Translations));
end Table_If;

with Ada.Text_IO;
with Templates_Parser;

procedure Matrix is

   package TP renames Templates_Parser;

   use type TP.Tag;

   V1 : constant TP.Vector_Tag := +"A1.1" & "A1.2";
   V2 : constant TP.Vector_Tag := +"A2.1" & "A2.2";
   V3 : constant TP.Vector_Tag := +"A3.1" & "A3.2";

   M  : constant TP.Matrix_Tag := +V1 & V2 & V3;

begin
   Ada.Text_IO.Put_Line
     (TP.Parse ("matrix.tmplt",
                TP.Translate_Table'(1 => TP.Assoc ("MAT", M))));
end Matrix;

with Ada.Text_IO;
with Templates_Parser;

procedure Macro is

   use type Templates_Parser.Vector_Tag;

   Translations : Templates_Parser.Translate_Set;

begin
   Templates_Parser.Insert
     (Translations,
      Templates_Parser.Assoc ("VAR", "Templates_Parser"));
   Ada.Text_IO.Put_Line
     (Templates_Parser.Parse ("macro.tmplt", Translations));
end Macro;
