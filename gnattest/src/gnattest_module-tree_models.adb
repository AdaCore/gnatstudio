------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2016-2018, AdaCore                  --
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

with Gtk.Tree_Model.Utils;
with System.Storage_Elements; use System.Storage_Elements;

package body GNATTest_Module.Tree_Models is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   procedure To_Row_Index
     (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Row    : out Row_Index;
      Length : out Natural);

   function To_Iter (Value : Natural) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Return Iter of first level

   function To_Iter (Value : Row_Index) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Return Iter of second level

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Row    : Row_Index;
      Length : Natural;
      Result : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      To_Row_Index (Parent, Row, Length);

      if Length = 0 and then not Self.Index.Is_Empty then

         --  First child of the root
         Result := To_Iter (0);
      elsif Length = 1 then

         --  First child of the first level row
         Result := To_Iter (Row);
      end if;

      return Result;
   end Children;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);
      use type Glib.Gint;
   begin
      if Index = File_Level_Column then
         return Glib.GType_Boolean;
      else
         return Glib.GType_String;
      end if;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Glib.Gint;

      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);

      Index_1 : constant Integer := Indices'First;
      Index_2 : constant Integer := Indices'First + 1;

      Row    : Row_Index;
      Result : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      case Indices'Length is
         when 1 =>

            if Self.Index.Is_Empty then
               return Result;
            end if;

            Row := Self.Index.Last_Key;

            if Integer (Indices (Index_1)) in 0 .. Row (1) then
               Result := To_Iter (Natural (Indices (Index_1)));
            end if;

         when 2 =>

            if Indices (Index_1) >= 0 and Indices (Index_2) >= 0 then
               Row := (Natural (Indices (Index_1)),
                       Natural (Indices (Index_2)));

               if Self.Index.Contains (Row) then
                  Result := To_Iter (Row);
               end if;
            end if;

         when others =>

            null;
      end case;

      return Result;
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Tree_Model_Record)
      return Glib.Gint
   is
      pragma Unreferenced (Self);
   begin
      return 4;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      pragma Unreferenced (Self);
      Result : Gtk.Tree_Model.Gtk_Tree_Path;
      Row    : Row_Index;
      Length : Natural;
   begin
      Gtk.Tree_Model.Gtk_New (Result);
      To_Row_Index (Iter, Row, Length);

      if Length > 0 then
         Gtk.Tree_Model.Append_Index (Result, Glib.Gint (Row (1)));

         if Length = 2 then
            Gtk.Tree_Model.Append_Index (Result, Glib.Gint (Row (2)));
         end if;
      end if;

      return Result;
   end Get_Path;

   -----------------------
   -- Get_Source_Entity --
   -----------------------

   not overriding function Get_Source_Entity
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Source_Entity
   is
      Row    : Row_Index;
      Length : Natural;
      Cursor : Source_Entity_Maps.Cursor;
      Result : Source_Entity;
   begin
      To_Row_Index (Iter, Row, Length);
      Cursor := Self.Index.Element (Row);
      Result := Source_Entity_Maps.Key (Cursor);

      return Result;
   end Get_Source_Entity;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      use type Glib.Gint;

      Row    : Row_Index;
      Length : Natural;
      Cursor : Source_Entity_Maps.Cursor;
      Src    : Source_Entity;
   begin
      To_Row_Index (Iter, Row, Length);

      if Column = First_Icon_Column then
         if Length = 1 then
            --  Draw setup icon on file's level
            Glib.Values.Init_Set_String
              (Value, "gps-emblem-entity-package-spec");
         else
            --  Draw source icon on test's level
            Glib.Values.Init_Set_String
              (Value, "gps-emblem-entity-subprogram-spec");
         end if;

         return;
      elsif Column = Second_Icon_Column then
         --  Draw teardown icon on file's level
         Glib.Values.Init_Set_String
           (Value, "gps-emblem-entity-package");
         return;
      elsif Column = File_Level_Column then
         Glib.Values.Init_Set_Boolean (Value, Length = 1);
         return;
      end if;

      Cursor := Self.Index.Element (Row);
      Src := Source_Entity_Maps.Key (Cursor);

      if Length = 1 then
         Glib.Values.Init_Set_String
           (Value,
            Src.Source_File.Display_Base_Name);
      elsif Length = 2 then
         Glib.Values.Init_Set_String
           (Value,
            To_String (Src.Subprogram_Name)
              & ":" & To_String (Src.Test_Case_Name));
      else
         Glib.Values.Init_Set_String (Value, "");
      end if;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model : out Tree_Model;
      Data  : Source_Entity_Maps.Map) is
   begin
      Model := new Tree_Model_Record;
      Initialize (Model, Data);
   end Gtk_New;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Row    : Row_Index;
      Length : Natural;
   begin
      To_Row_Index (Iter, Row, Length);

      return Length in 0 | 1 and then not Self.Index.Is_Empty;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : access Tree_Model_Record'Class;
      Data : Source_Entity_Maps.Map)
   is
      Row    : Row_Index := (0, 0);
      Cursor : Source_Entity_Maps.Cursor := Data.First;
      File   : Virtual_File := No_File;
      Src    : Source_Entity;
   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      Self.Index.Clear;
      --  Self.Data := Data;

      while Source_Entity_Maps.Has_Element (Cursor) loop
         Src := Source_Entity_Maps.Key (Cursor);

         --  Avoid setup/teardown items in the model
         if Src.Test_Case_Name not in Test_Setup | Test_Teardown then
            if File = Src.Source_File then
               Row (2) := Row (2) + 1;
            elsif File = No_File then
               --  For first iteration keep Row as (0, 0)
               Row := (0, 0);
               File := Src.Source_File;
            else
               Row := (Row (1) + 1, 0);
               File := Src.Source_File;
            end if;

            Self.Index.Insert (Row, Cursor);
         end if;

         Source_Entity_Maps.Next (Cursor);
      end loop;
   end Initialize;

   ----------
   -- Less --
   ----------

   function Less (Left, Right : Row_Index) return Boolean is
   begin
      return (Left (1) = Right (1) and Left (2) < Right (2))
        or Left (1) < Right (1);
   end Less;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Row    : Row_Index;
      Length : Natural;
      Result : Glib.Gint := 0;
   begin
      To_Row_Index (Iter, Row, Length);

      if Length = 0 and then not Self.Index.Is_Empty then

         Result := Glib.Gint (Self.Index.Last_Key (1) + 1);
      elsif Length = 1 then
         Row (2) := Natural'Last;
         Row := Index_Ordered_Maps.Key (Self.Index.Floor (Row));
         Result := Glib.Gint (Row (2) + 1);
      end if;

      return Result;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Row    : Row_Index;
      Length : Natural;
   begin
      To_Row_Index (Iter, Row, Length);
      Iter := Gtk.Tree_Model.Null_Iter;

      if Length = 1 then
         Row (1) := Row (1) + 1;

         if Self.Index.Contains (Row) then
            Iter := To_Iter (Row (1));
         end if;
      elsif Length = 2 then

         Row (2) := Row (2) + 1;

         if Self.Index.Contains (Row) then
            Iter := To_Iter (Row);
         end if;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      use type Glib.Gint;

      Row    : Row_Index;
      Length : Natural;
      Result : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      if N < Self.N_Children (Parent) then
         To_Row_Index (Parent, Row, Length);

         if Length = 0 then

            Result := To_Iter (Natural (N));
         elsif Length = 1 then

            Row (2) := Natural (N);
            Result := To_Iter (Row);
         end if;
      end if;

      return Result;
   end Nth_Child;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Unreferenced (Self);

      Row    : Row_Index;
      Length : Natural;
      Result : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter;
   begin
      To_Row_Index (Child, Row, Length);

      if Length = 2 then

         Result := To_Iter (Row (1));
      end if;

      return Result;
   end Parent;

   -------------
   -- To_Iter --
   -------------

   function To_Iter (Value : Natural) return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Gtk.Tree_Model.Utils.Init_Tree_Iter
              (Stamp       => 1,
               User_Data_1 => To_Address
                                (Integer_Address (Value)),
               User_Data_2 => To_Address (Integer_Address'Last));
   end To_Iter;

   -------------
   -- To_Iter --
   -------------

   function To_Iter (Value : Row_Index) return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Gtk.Tree_Model.Utils.Init_Tree_Iter
              (Stamp       => 2,
               User_Data_1 => To_Address
                                (Integer_Address (Value (1))),
               User_Data_2 => To_Address
                                (Integer_Address (Value (2))));
   end To_Iter;

   ------------------
   -- To_Row_Index --
   ------------------

   procedure To_Row_Index
     (Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Row    : out Row_Index;
      Length : out Natural)
   is
      Data_1 : constant Integer_Address := To_Integer
        (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter));

      Data_2 : constant Integer_Address := To_Integer
        (Gtk.Tree_Model.Utils.Get_User_Data_2 (Iter));
   begin
      if Iter /= Gtk.Tree_Model.Null_Iter then
         Row (1) := Natural (Data_1);

         if Data_2 /= Integer_Address'Last then

            Row (2) := Natural (Data_2);
            Length := 2;
         else

            Row (2) := 0;
            Length := 1;
         end if;
      else

         Length := 0;
         Row := (0, 0);
      end if;
   end To_Row_Index;

end GNATTest_Module.Tree_Models;
