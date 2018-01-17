------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2011-2018, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;

with Gtk.Tree_Model;        use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;

with GPS.Editors.GtkAda;

package body CodePeer.Race_Details_Models is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   function To_Iter (Index : Natural) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function From_Iter (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Natural;

   ---------------
   -- From_Iter --
   ---------------

   function From_Iter (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Natural is
      pragma Warnings (Off);
      function To_Integer is
        new Ada.Unchecked_Conversion (System.Address, Integer);
      pragma Warnings (On);

   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return 0;

      else
         return To_Integer (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter));
      end if;
   end From_Iter;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Race_Details_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Entry_Point_Name_Column =>
            return Glib.GType_String;

         when Access_Kind_Column =>
            return Glib.GType_String;

         when Mark_Column =>
            return GPS.Editors.GtkAda.Get_Editor_Mark_Type;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Race_Details_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
      Index   : Natural;

   begin
      if Indices'Length = 1 then
         Index := Natural (Indices (Indices'First)) + 1;

         if Index in Self.Data.First_Index .. Self.Data.Last_Index then
            return To_Iter (Index);
         end if;
      end if;

      return Gtk.Tree_Model.Null_Iter;
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Race_Details_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Number_Of_Columns;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Race_Details_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Index : constant Natural := From_Iter (Iter);
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      if Iter /= Gtk.Tree_Model.Null_Iter
        and then Index in Self.Data.First_Index .. Self.Data.Last_Index
      then
         Gtk.Tree_Model.Gtk_New (Path);
         Gtk.Tree_Model.Append_Index (Path, Glib.Gint (Index - 1));

      else
         Path := Null_Gtk_Tree_Path;
      end if;

      return Path;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Race_Details_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Index : constant Natural := From_Iter (Iter);

   begin
      if Iter /= Gtk.Tree_Model.Null_Iter
        and then Index in Self.Data.First_Index .. Self.Data.Last_Index
      then
         case Column is
            when Entry_Point_Name_Column =>
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String
                 (Value,
                  To_String (Self.Data.Element (Index).Entry_Point.Name));

            when Access_Kind_Column =>
               Glib.Values.Init (Value, Glib.GType_String);

               case Self.Data (Index).Object_Access.Kind is
                  when Read =>
                     Glib.Values.Set_String (Value, "READ");

                  when Update =>
                     Glib.Values.Set_String (Value, "UPDATE");
               end case;

            when Mark_Column =>
               Glib.Values.Init
                 (Value, GPS.Editors.GtkAda.Get_Editor_Mark_Type);
               GPS.Editors.GtkAda.Set_Mark
                 (Value,
                  Self.Kernel.Get_Buffer_Factory.New_Mark
                    (Self.Data (Index).Object_Access.File,
                     Self.Data (Index).Object_Access.Line,
                     Self.Data (Index).Object_Access.Column));

            when others =>
               null;
         end case;
      end if;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model  : out Race_Details_Model;
      Kernel : not null GPS.Kernel.Kernel_Handle) is
   begin
      Model := new Race_Details_Model_Record;
      Initialize (Model, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Race_Details_Model_Record'Class;
      Kernel : not null GPS.Kernel.Kernel_Handle) is
   begin
      Gtkada.Abstract_List_Model.Initialize (Self);
      Self.Kernel := Kernel;
   end Initialize;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Race_Details_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Glib.Gint is
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Glib.Gint (Self.Data.Length);

      else
         return 0;
      end if;
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Race_Details_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Index : constant Natural := From_Iter (Iter) + 1;

   begin
      if Iter /= Gtk.Tree_Model.Null_Iter
        and then Index in Self.Data.First_Index .. Self.Data.Last_Index
      then
         Iter := To_Iter (Index);

      else
         Iter := Gtk.Tree_Model.Null_Iter;
      end if;
   end Next;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Race_Details_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Index : constant Natural := Natural (N) + 1;

   begin
      if Parent = Gtk.Tree_Model.Null_Iter
        and then Index in Self.Data.First_Index .. Self.Data.Last_Index
      then
         return To_Iter (Index);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Nth_Child;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : not null access Race_Details_Model_Record'Class;
      Data : CodePeer.Entry_Point_Object_Access_Vectors.Vector)
   is
      Info : Entry_Point_Object_Access_Information;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      --  Clear current content of the model and notify view.

      while not Self.Data.Is_Empty loop
         Iter := To_Iter (Self.Data.Last_Index);
         Path := Self.Get_Path (Iter);
         Self.Data.Delete_Last;
         Row_Deleted (To_Interface (Self), Path);
         Gtk.Tree_Model.Path_Free (Path);
      end loop;

      --  Transform data into internal representation

      for J in Data.First_Index .. Data.Last_Index loop
         Info := Data.Element (J);

         for K in
           Info.Object_Accesses.First_Index .. Info.Object_Accesses.Last_Index
         loop
            Self.Data.Append
              ((Info.Entry_Point, Info.Object_Accesses (K)));
            Iter := To_Iter (Self.Data.Last_Index);
            Path := Self.Get_Path (Iter);
            Row_Inserted (To_Interface (Self), Path, Iter);
            Gtk.Tree_Model.Path_Free (Path);
         end loop;
      end loop;
   end Set;

   -------------
   -- To_Iter --
   -------------

   function To_Iter (Index : Natural) return Gtk.Tree_Model.Gtk_Tree_Iter is
      pragma Warnings (Off);
      function To_Address is
        new Ada.Unchecked_Conversion (Integer, System.Address);
      pragma Warnings (On);

   begin
      if Index = 0 then
         return Gtk.Tree_Model.Null_Iter;

      else
         return Gtk.Tree_Model.Utils.Init_Tree_Iter (1, To_Address (Index));
      end if;
   end To_Iter;

end CodePeer.Race_Details_Models;
