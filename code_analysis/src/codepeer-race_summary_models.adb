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
with System.Address_To_Access_Conversions;

with Gtk.Tree_Model.Utils;
with GPS.Kernel.Project;

package body CodePeer.Race_Summary_Models is

   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   function To_Iter (Index : Natural) return Gtk.Tree_Model.Gtk_Tree_Iter;

   function From_Iter (Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Natural;

   package Message_Conversions is
     new System.Address_To_Access_Conversions
       (GPS.Kernel.Messages.Abstract_Message'Class);

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
     (Self  : access Race_Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Object_Name_Column =>
            return Glib.GType_String;

         when Message_Column =>
            return Glib.GType_Pointer;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   ----------------------
   -- Get_Entry_Points --
   ----------------------

   function Get_Entry_Points
     (Self : not null access Race_Summary_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return CodePeer.Entry_Point_Object_Access_Vectors.Vector
   is
      Index : constant Natural := From_Iter (Iter);

   begin
      if Iter /= Gtk.Tree_Model.Null_Iter
        and then Index in Self.Data.First_Index .. Self.Data.Last_Index
      then
         return Self.Data.Element (Index).Entry_Points;

      else
         return CodePeer.Entry_Point_Object_Access_Vectors.Empty_Vector;
      end if;
   end Get_Entry_Points;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Race_Summary_Model_Record;
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
     (Self : access Race_Summary_Model_Record)
      return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Total_Columns;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Race_Summary_Model_Record;
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
         Path := Gtk.Tree_Model.Null_Gtk_Tree_Path;
      end if;

      return Path;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Race_Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Index : constant Natural := From_Iter (Iter);

   begin
      if Index in Self.Data.First_Index .. Self.Data.Last_Index then
         case Column is
            when Object_Name_Column =>
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String
                 (Value, To_String (Self.Data.Element (Index).Name));

            when Message_Column =>
               Glib.Values.Init (Value, Glib.GType_Pointer);
               Glib.Values.Set_Address
                 (Value,
                  Message_Conversions.To_Address
                    (Message_Conversions.Object_Pointer
                         (Self.Data (Index).Message)));

            when others =>
               null;
         end case;

      else
         Glib.Values.Init (Value, Glib.GType_Invalid);
      end if;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model  : out Race_Summary_Model;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree) is
   begin
      Model := new Race_Summary_Model_Record;
      Initialize (Model, Kernel, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : not null access Race_Summary_Model_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : Code_Analysis.Code_Analysis_Tree)
   is
      Project_Data : CodePeer.Project_Data'Class renames
        CodePeer.Project_Data'Class
          (Code_Analysis.Get_Or_Create
               (Tree,
                GPS.Kernel.Project.Get_Project
                  (Kernel)).Analysis_Data.CodePeer_Data.all);

   begin
      Gtkada.Abstract_List_Model.Initialize (Self);
      Self.Data := Project_Data.Object_Races;
   end Initialize;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Race_Summary_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint is
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
     (Self : access Race_Summary_Model_Record;
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
     (Self   : access Race_Summary_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint)
      return Gtk.Tree_Model.Gtk_Tree_Iter
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

end CodePeer.Race_Summary_Models;
