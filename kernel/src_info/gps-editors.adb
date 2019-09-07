------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

package body GPS.Editors is

   ---------
   -- "=" --
   ---------

   overriding function "="
     (This : Editor_Buffer; Buffer : Editor_Buffer) return Boolean is
   begin
      --  ??? this body is provided so that the type is correctly bound to
      --  java, waiting for J617-004 to be removed
      raise Program_Error with "Not implemented";
      return False;
   end "=";

   --  Dummy bodies for implementation of Nil values.

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
     (This : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class
   is
      pragma Unreferenced (This, Update_Tree);
   begin
      return Nil_Editor_Location;
   end Block_Start;

   overriding function Block_End
     (This : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Editor_Location'Class
   is
      pragma Unreferenced (This, Update_Tree);
   begin
      return Nil_Editor_Location;
   end Block_End;

   overriding function Block_Type
     (This : Dummy_Editor_Location;
      Update_Tree : Boolean := True) return Language_Category
   is
      pragma Unreferenced (This, Update_Tree);
   begin
      return Cat_Unknown;
   end Block_Type;

   overriding function Line (This : Dummy_Editor_Location) return Integer is
      pragma Unreferenced (This);
   begin
      return 0;
   end Line;

   overriding function Column
     (This : Dummy_Editor_Location) return Visible_Column_Type
   is
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

   overriding function Column
     (This : Dummy_Editor_Mark) return Visible_Column_Type is
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
      Column : Visible_Column_Type) return Editor_Location'Class
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
     (This                 : Dummy_Editor_Buffer;
      From                 : Editor_Location'Class := Nil_Editor_Location;
      To                   : Editor_Location'Class := Nil_Editor_Location;
      Include_Hidden_Chars : Boolean := True) return String
   is
      pragma Unreferenced (This, From, To, Include_Hidden_Chars);
   begin
      return "";
   end Get_Chars;

   ---------------------
   -- Get_Entity_Name --
   ---------------------

   overriding function Get_Entity_Name
     (This     : Dummy_Editor_Buffer;
      Location : Editor_Location'Class := Nil_Editor_Location)
      return String
   is
      pragma Unreferenced (This, Location);
   begin
      return "";
   end Get_Entity_Name;

   overriding function Get_Chars_U
     (This                 : Dummy_Editor_Buffer;
      From                 : Editor_Location'Class := Nil_Editor_Location;
      To                   : Editor_Location'Class := Nil_Editor_Location;
      Include_Hidden_Chars : Boolean := True) return Unbounded_String
   is
      pragma Unreferenced (This, From, To, Include_Hidden_Chars);
   begin
      return Null_Unbounded_String;
   end Get_Chars_U;

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

   -----------------
   -- Is_Modified --
   -----------------

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
      C1 : Visible_Column_Type;
      C2 : Visible_Column_Type;
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

   ----------
   -- Name --
   ----------

   overriding function Name (This : Dummy_Editor_Mark) return String is
      pragma Unreferenced (This);
   begin
      return "";
   end Name;

   ---------------------
   -- Create_Instance --
   ---------------------

   overriding function Create_Instance
     (This   : Dummy_Editor_Mark;
      Script : access Scripting_Language_Record'Class)
      return Class_Instance
   is
      pragma Unreferenced (This, Script);
   begin
      return No_Class_Instance;
   end Create_Instance;

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

   -----------------
   -- Inside_Word --
   -----------------

   overriding function Inside_Word
     (This : Dummy_Editor_Location) return Boolean is
      pragma Unreferenced (This);
   begin
      return False;
   end Inside_Word;

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
     (This        : Dummy_Editor_Location;
      Subprogram  : Boolean;
      Update_Tree : Boolean := True) return String
   is
      pragma Unreferenced (This, Subprogram, Update_Tree);
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

   ---------------------
   -- Create_Instance --
   ---------------------

   overriding function Create_Instance
     (This   : Dummy_Editor_Location;
      Script : access Scripting_Language_Record'Class) return Class_Instance
   is
      pragma Unreferenced (This, Script);
   begin
      return No_Class_Instance;
   end Create_Instance;

   --------------------------
   -- New_Location_At_Line --
   --------------------------

   function New_Location_At_Line
     (This   : Editor_Buffer;
      Line   : Integer) return Editor_Location'Class
   is
   begin
      return Editor_Buffer'Class
        (This).New_Location (Line, Visible_Column_Type'(1));
   end New_Location_At_Line;

   ------------------------
   -- Current_Undo_Group --
   ------------------------

   pragma Warnings (Off); --  Kill the "unreachable code" warning

   function Current_Undo_Group
     (This : Editor_Buffer) return Group_Block is
   begin
      raise Program_Error; --  Intended: this should be overriden
      return G : Group_Block do null; end return;
   end Current_Undo_Group;

   --------------------
   -- New_Undo_Group --
   --------------------

   function New_Undo_Group (This : Editor_Buffer) return Group_Block is
   begin
      raise Program_Error; --  Intended: this should be overriden
      return G : Group_Block do null; end return;
   end New_Undo_Group;

   pragma Warnings (On);

end GPS.Editors;
