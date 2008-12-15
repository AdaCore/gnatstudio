-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2008, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with GPS.Intl; use GPS.Intl;
with Projects;

with Code_Peer.Utilities;

package body Code_Peer.Summary_Models is

   use type Code_Analysis.File_Access;
   use type Code_Analysis.Project_Access;
   use type Code_Analysis.Subprogram_Access;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Summary_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when Entity_Name_Column
            | Informational_Count_Column
            | Low_Count_Column
            | Medium_Count_Column
            | High_Count_Column
            | Suppressed_Count_Column
              =>
            return Glib.GType_String;

         when others =>
            return Glib.GType_Invalid;
      end case;
   end Get_Column_Type;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Summary_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return Number_Of_Columns;
   end Get_N_Columns;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Summary_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Project_Node    : constant Code_Analysis.Project_Access :=
                          Self.Project_At (Iter);
      File_Node       : constant Code_Analysis.File_Access :=
                          Self.File_At (Iter);
      Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
                          Self.Subprogram_At (Iter);

      procedure Set_Integer_Image (Item : Natural);

      procedure Set_Count_Image (Level : Code_Peer.Message_Probability_Level);

      -----------------------
      -- Set_Integer_Image --
      -----------------------

      procedure Set_Count_Image
        (Level : Code_Peer.Message_Probability_Level) is
      begin
         if Subprogram_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Subprogram_Node, Level));

         elsif File_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (File_Node, Level));

         elsif Project_Node /= null then
            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Project_Node, Level));

         else
            --  "Total" line

            Set_Integer_Image
              (Code_Peer.Utilities.Compute_Messages_Count
                 (Self.Tree, Level));
         end if;
      end Set_Count_Image;

      -----------------------
      -- Set_Integer_Image --
      -----------------------

      procedure Set_Integer_Image (Item : Natural) is
         Image : constant String := Natural'Image (Item);

      begin
         Glib.Values.Init (Value, Glib.GType_String);

         if Item = 0 then
            Glib.Values.Set_String (Value, "");

         else
            Glib.Values.Set_String
              (Value, Image (Image'First + 1 .. Image'Last));
         end if;
      end Set_Integer_Image;

   begin
      case Column is
         when Entity_Name_Column =>
            if Subprogram_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, Subprogram_Node.Name.all);

            elsif File_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, File_Node.Name.Base_Name);

            elsif Project_Node /= null then
               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String
                 (Value, Projects.Project_Name (Project_Node.Name));

            else
               --  "Total" line

               Glib.Values.Init (Value, Glib.GType_String);
               Glib.Values.Set_String (Value, -"Total:");
            end if;

         when Informational_Count_Column =>
            Set_Count_Image (Code_Peer.Informational);

         when Low_Count_Column =>
            Set_Count_Image (Code_Peer.Low);

         when Medium_Count_Column =>
            Set_Count_Image (Code_Peer.Medium);

         when High_Count_Column =>
            Set_Count_Image (Code_Peer.High);

         when Suppressed_Count_Column =>
            Set_Count_Image (Code_Peer.Suppressed);

         when others =>
            null;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Model : out Summary_Model;
      Tree  : Code_Analysis.Code_Analysis_Tree) is
   begin
      Model := new Summary_Model_Record;
      Initialize (Model, Tree);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Model : access Summary_Model_Record'Class;
      Tree  : Code_Analysis.Code_Analysis_Tree) is
   begin
      Code_Analysis.Tree_Models.Initialize (Model, Tree);

      Model.Tree := Tree;
   end Initialize;

end Code_Peer.Summary_Models;
