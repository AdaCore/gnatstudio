-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2008-2009, AdaCore               --
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

package body GPS.Editors is

   use Commands;

   --  Dummy bodies for implementation of Nil values.

   procedure Add_Special_Line
     (This       : Editor_Buffer'Class;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null)
   is
      Mark : constant Editor_Mark'Class :=
        This.Add_Special_Line
          (Start_Line, Text, Category, Name, Column_Id, Info);
      pragma Unreferenced (Mark);

   begin
      null;
   end Add_Special_Line;

   overriding function Beginning_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Beginning_Of_Line;

   overriding function End_Of_Line
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end End_Of_Line;

   overriding function Block_Start
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Block_Start;

   overriding function Block_End
     (This : Dummy_Editor_Location) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Block_End;

   overriding function Block_Type
     (This : Dummy_Editor_Location) return Language_Category
   is
      pragma Unreferenced (This);
   begin
      return Cat_Unknown;
   end Block_Type;

   overriding function Line (This : Dummy_Editor_Location) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Line;

   overriding function Column (This : Dummy_Editor_Location) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Column;

   overriding function Buffer
     (This : Dummy_Editor_Location) return Editor_Buffer'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Buffer;
   end Buffer;

   overriding function Create_Mark
     (This : Dummy_Editor_Location; Name : String := "")
      return Editor_Mark'Class
   is
      pragma Unreferenced (This, Name);
   begin
      return Nil_Editor_Mark;
   end Create_Mark;

   overriding function Forward_Char
     (This : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Count);
   begin
      return Nil_Editor_Location;
   end Forward_Char;

   overriding function Line (This : Dummy_Editor_Mark) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Line;

   overriding function Column (This : Dummy_Editor_Mark) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Column;

   overriding function Location
     (This : Dummy_Editor_Mark;
      Open : Boolean) return Editor_Location'Class
   is
      pragma Unreferenced (This, Open);
   begin
      return Nil_Editor_Location;
   end Location;

   overriding function New_Location
     (This   : Dummy_Editor_Buffer;
      Line   : Integer;
      Column : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Line, Column);
   begin
      return Nil_Editor_Location;
   end New_Location;

   overriding function New_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_View;
   end New_View;

   overriding function Open
     (This : Dummy_Editor_Buffer) return Editor_View'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_View;
   end Open;

   overriding function Add_Special_Line
     (This       : Dummy_Editor_Buffer;
      Start_Line : Integer;
      Text       : String;
      Category   : String := "";
      Name       : String := "";
      Column_Id  : String := "";
      Info       : Line_Information_Data := null) return Editor_Mark'Class
   is
      pragma Unreferenced
        (This, Start_Line, Text, Category, Name, Column_Id, Info);
   begin
      return Nil_Editor_Mark;
   end Add_Special_Line;

   overriding function Current_View
     (This : Dummy_Editor_Buffer) return Editor_View'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_View;
   end Current_View;

   overriding function Lines_Count
     (This : Dummy_Editor_Buffer) return Integer
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Lines_Count;

   overriding function Characters_Count
     (This : Dummy_Editor_Buffer) return Natural
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Characters_Count;

   overriding function Get_Chars
     (This : Dummy_Editor_Buffer;
      From : Editor_Location'Class := Nil_Editor_Location;
      To   : Editor_Location'Class := Nil_Editor_Location) return String
   is
      pragma Unreferenced (This, From, To);
   begin
      return "";
   end Get_Chars;

   overriding function Beginning_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Beginning_Of_Buffer;

   overriding function End_Of_Buffer
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end End_Of_Buffer;

   overriding function Get_Mark
     (This : Dummy_Editor_Buffer;
      Name : String) return Editor_Mark'Class
   is
      pragma Unreferenced (This, Name);
   begin
      return Nil_Editor_Mark;
   end Get_Mark;

   overriding function Is_Present (This : Dummy_Editor_Mark) return Boolean is
      pragma Unreferenced (This);

   begin
      return False;
   end Is_Present;

   overriding function Cursor
     (This : Dummy_Editor_View) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Cursor;

   overriding procedure Get_Constructs
     (This       : Dummy_Editor_Buffer;
      Constructs : out Language.Construct_List;
      Timestamp  : out Natural)
   is
      pragma Unreferenced (This);
   begin
      Constructs := (null, null, null, 0);
      Timestamp := 0;
   end Get_Constructs;

   --------------------
   -- Set_Foreground --
   --------------------

   procedure Set_Foreground
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Foreground);
      Style.Foreground := new String'(Color);
   end Set_Foreground;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background
     (Style : not null access Simple_Style_Record; Color : String) is
   begin
      Free (Style.Background);
      Style.Background := new String'(Color);
   end Set_Background;

   --------------------
   -- Get_Background --
   --------------------

   function Get_Background
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Background = null then
         return "";
      else
         return Style.Background.all;
      end if;
   end Get_Background;

   --------------------
   -- Get_Foreground --
   --------------------

   function Get_Foreground
     (Style : not null access Simple_Style_Record) return String is
   begin
      if Style.Foreground = null then
         return "";
      else
         return Style.Foreground.all;
      end if;
   end Get_Foreground;

   ----------
   -- Free --
   ----------

   procedure Free (Style : in out Simple_Style_Record) is
   begin
      Free (Style.Background);
      Free (Style.Foreground);
   end Free;

   ---------------------
   -- Set_In_Speedbar --
   ---------------------

   procedure Set_In_Speedbar
     (Style       : not null access Simple_Style_Record;
      In_Speedbar : Boolean) is
   begin
      Style.Speedbar := In_Speedbar;
   end Set_In_Speedbar;

   -----------------
   -- In_Speedbar --
   -----------------

   function In_Speedbar
     (Style       : not null access Simple_Style_Record) return Boolean is
   begin
      return Style.Speedbar;
   end In_Speedbar;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Record) is
   begin
      Free (X.Text);
      Free (X.Tooltip_Text);

      if X.Associated_Command /= null then
         Unref (X.Associated_Command);
      end if;
   end Free;

   -------------------
   -- Get_MDI_Child --
   -------------------

   overriding function Get_MDI_Child
     (This : Dummy_Editor_View) return System.Address is
      pragma Unreferenced (This);
   begin
      return System.Null_Address;
   end Get_MDI_Child;

   ----------
   -- File --
   ----------

   overriding function File (This : Dummy_Editor_Buffer) return Virtual_File is
      pragma Unreferenced (This);
   begin
      return No_File;
   end File;

   ------------------
   -- Is_Read_Only --
   ------------------

   overriding function Is_Read_Only
     (This : Dummy_Editor_Buffer) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return False;  --  Most operations would raise an error if we return True
   end Is_Read_Only;

   ---------------------
   -- Selection_Start --
   ---------------------

   overriding function Selection_Start
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Selection_Start;

   -------------------
   -- Selection_End --
   -------------------

   overriding function Selection_End
     (This : Dummy_Editor_Buffer) return Editor_Location'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Location;
   end Selection_End;

   overriding function Is_Modified
     (This : Dummy_Editor_Buffer) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return False;
   end Is_Modified;

   -------------
   -- Compare --
   -------------

   function Compare
     (This : Editor_Location; To : Editor_Location) return Compare_Result
   is
      L1 : constant Integer := Line (Editor_Location'Class (This));
      L2 : constant Integer := Line (Editor_Location'Class (To));
      C1 : Integer;
      C2 : Integer;
   begin
      if L1 < L2 then
         return -1;
      elsif L1 = L2 then
         C1 := Column (Editor_Location'Class (This));
         C2 := Column (Editor_Location'Class (To));

         if C1 < C2 then
            return -1;
         elsif C1 = C2 then
            return 0;
         else
            return 1;
         end if;
      else
         return 1;
      end if;
   end Compare;

   ------------------
   -- Is_Read_Only --
   ------------------

   overriding function Is_Read_Only
     (This : Dummy_Editor_View) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return False;
   end Is_Read_Only;

   -----------
   -- Title --
   -----------

   overriding function Title
     (This : Dummy_Editor_View; Short : Boolean) return String
   is
      pragma Unreferenced (This, Short);
   begin
      return "";
   end Title;

   ------------
   -- Buffer --
   ------------

   overriding function Buffer
     (This : Dummy_Editor_View) return Editor_Buffer'Class
   is
      pragma Unreferenced (This);
   begin
      return Nil_Editor_Buffer;
   end Buffer;

   --------------
   -- Get_Name --
   --------------

   overriding function Name (This : Dummy_Editor_Mark) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Name;

   ------------------
   -- Forward_Word --
   ------------------

   overriding function Forward_Word
     (This  : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Count);
   begin
      return Nil_Editor_Location;
   end Forward_Word;

   ------------------
   -- Forward_Line --
   ------------------

   overriding function Forward_Line
     (This  : Dummy_Editor_Location;
      Count : Integer) return Editor_Location'Class
   is
      pragma Unreferenced (This, Count);
   begin
      return Nil_Editor_Location;
   end Forward_Line;

   -----------------
   -- Starts_Word --
   -----------------

   overriding function Starts_Word
     (This : Dummy_Editor_Location) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return False;
   end Starts_Word;

   ---------------
   -- Ends_Word --
   ---------------

   overriding function Ends_Word
     (This : Dummy_Editor_Location) return Boolean
   is
      pragma Unreferenced (This);
   begin
      return False;
   end Ends_Word;

   --------------
   -- Get_Char --
   --------------

   overriding function Get_Char
     (This : Dummy_Editor_Location) return Integer
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Get_Char;

   ----------------
   -- Block_Name --
   ----------------

   overriding function Block_Name
     (This : Dummy_Editor_Location; Subprogram : Boolean) return String
   is
      pragma Unreferenced (This, Subprogram);
   begin
      return "";
   end Block_Name;

   -----------------
   -- Block_Level --
   -----------------

   overriding function Block_Level
     (This : Dummy_Editor_Location) return Natural
   is
      pragma Unreferenced (This);
   begin
      return 0;
   end Block_Level;

   ------------
   -- Offset --
   ------------

   overriding function Offset (This : Dummy_Editor_Location) return Natural is
      pragma Unreferenced (This);
   begin
      return 0;
   end Offset;

   ------------
   -- Search --
   ------------

   overriding procedure Search
     (This              : Dummy_Editor_Location;
      Pattern           : String;
      Backward          : Boolean := False;
      Case_Sensitive    : Boolean := False;
      Regexp            : Boolean := False;
      Whole_Word        : Boolean := False;
      Scope             : String := "Whole";
      Dialog_On_Failure : Boolean := True;
      Success           : out Boolean;
      Starts            : out Dummy_Editor_Location;
      Ends              : out Dummy_Editor_Location)
   is
      pragma Unreferenced (This, Pattern, Backward, Case_Sensitive, Regexp,
                           Whole_Word, Scope, Dialog_On_Failure);
   begin
      Success := False;
      Starts  := Dummy_Editor_Location (Nil_Editor_Location);
      Ends    := Dummy_Editor_Location (Nil_Editor_Location);
   end Search;

   --------------------
   -- Create_Overlay --
   --------------------

   overriding function Create_Overlay
     (This : Dummy_Editor_Buffer;
      Name : String := "") return Editor_Overlay'Class
   is
      pragma Unreferenced (This, Name);
   begin
      return Nil_Editor_Overlay;
   end Create_Overlay;

   ----------
   -- Name --
   ----------

   overriding function Name (This : Dummy_Editor_Overlay) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Name;

   ------------------
   -- Get_Property --
   ------------------

   overriding function Get_Property
     (This : Dummy_Editor_Overlay; Name : String) return String
   is
      pragma Unreferenced (This, Name);
   begin
      return "";
   end Get_Property;

   overriding function Get_Property
     (This : Dummy_Editor_Overlay; Name : String) return Boolean
   is
      pragma Unreferenced (This, Name);
   begin
      return False;
   end Get_Property;

   -----------------
   -- Has_Overlay --
   -----------------

   overriding function Has_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Boolean
   is
      pragma Unreferenced (This, Overlay);
   begin
      return False;
   end Has_Overlay;

   ---------------------
   -- Forward_Overlay --
   ---------------------

   overriding function Forward_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class
   is
      pragma Unreferenced (This, Overlay);
   begin
      return Nil_Editor_Location;
   end Forward_Overlay;

   ----------------------
   -- Backward_Overlay --
   ----------------------

   overriding function Backward_Overlay
     (This    : Dummy_Editor_Location;
      Overlay : Editor_Overlay'Class) return Editor_Location'Class
   is
      pragma Unreferenced (This, Overlay);
   begin
      return Nil_Editor_Location;
   end Backward_Overlay;

   ------------------
   -- Get_Overlays --
   ------------------

   overriding function Get_Overlays
     (This    : Dummy_Editor_Location) return Overlay_Lists.List
   is
      pragma Unreferenced (This);
   begin
      return Overlay_Lists.Empty_List;
   end Get_Overlays;

   -----------
   -- Views --
   -----------

   overriding function Views
     (This : Dummy_Editor_Buffer) return View_Lists.List
   is
      pragma Unreferenced (This);
   begin
      return View_Lists.Empty_List;
   end Views;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (This : Dummy_Editor_Buffer; Buffer : Dummy_Editor_Buffer) return Boolean
   is
      pragma Unreferenced (This, Buffer);
   begin
      --  Always equal, since the only dummy_editor is nil_editor_buffer
      return True;
   end "=";

end GPS.Editors;
