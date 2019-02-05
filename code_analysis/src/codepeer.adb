------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Deallocation;

with String_Utils;

with Gtkada.Style;

package body CodePeer is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Project_Data) is

      procedure Process_Message_Category
        (Position : Message_Category_Sets.Cursor);

      procedure Process_Annotation_Category
        (Position : Annotation_Category_Sets.Cursor);

      procedure Process_Entry_Point
        (Position : Entry_Point_Information_Sets.Cursor);
      --  Deallocates entry point information

      ---------------------------------
      -- Process_Annotation_Category --
      ---------------------------------

      procedure Process_Annotation_Category
        (Position : Annotation_Category_Sets.Cursor)
      is
         Element : Annotation_Category_Access :=
                     Annotation_Category_Sets.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation_Category, Annotation_Category_Access);

      begin
         Free (Element);
      end Process_Annotation_Category;

      -------------------------
      -- Process_Entry_Point --
      -------------------------

      procedure Process_Entry_Point
        (Position : Entry_Point_Information_Sets.Cursor)
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (Entry_Point_Information, Entry_Point_Information_Access);

         Element : Entry_Point_Information_Access
           := Entry_Point_Information_Sets.Element (Position);

      begin
         Free (Element);
      end Process_Entry_Point;

      ------------------------------
      -- Process_Message_Category --
      ------------------------------

      procedure Process_Message_Category
        (Position : Message_Category_Sets.Cursor)
      is
         Element : Message_Category_Access :=
                     Message_Category_Sets.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Message_Category, Message_Category_Access);

      begin
         Free (Element);
      end Process_Message_Category;

   begin
      Self.Message_Categories.Iterate (Process_Message_Category'Access);
      Self.Annotation_Categories.Iterate (Process_Annotation_Category'Access);
      Self.Entry_Points.Iterate (Process_Entry_Point'Access);
      Self.Object_Races.Clear;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : access Subprogram_Data) is

      procedure Process_Annotations (Position : Annotation_Maps.Cursor);

      procedure Process_Annotation (Position : Annotation_Vectors.Cursor);

      ------------------------
      -- Process_Annotation --
      ------------------------

      procedure Process_Annotation (Position : Annotation_Vectors.Cursor) is
         Element : Annotation_Access := Annotation_Vectors.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation, Annotation_Access);

      begin
         Free (Element);
      end Process_Annotation;

      -------------------------
      -- Process_Annotations --
      -------------------------

      procedure Process_Annotations (Position : Annotation_Maps.Cursor) is
         Element : Annotation_Vector_Access :=
                     Annotation_Maps.Element (Position);

         procedure Free is new Ada.Unchecked_Deallocation
           (Annotation_Vectors.Vector, Annotation_Vector_Access);

      begin
         Element.Iterate (Process_Annotation'Access);
         Element.Clear;
         Free (Element);
      end Process_Annotations;

   begin
      Self.Messages.Clear;
      Self.Annotations.Iterate (Process_Annotations'Access);
      Self.Annotations.Clear;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : not null access Message) is

      procedure Free is
        new Ada.Unchecked_Deallocation (Audit_Record, Audit_Record_Access);

   begin
      for J of Self.Audit loop
         Free (J);
      end loop;

      Self.Audit.Clear;

      GPS.Kernel.Messages.Primary_Abstract_Message (Self.all).Finalize;
   end Finalize;

   ----------------
   -- Get_Markup --
   ----------------

   overriding function Get_Markup
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String is
   begin
      return Result : Ada.Strings.Unbounded.Unbounded_String :=
        GPS.Kernel.Messages.Abstract_Message (Self.all).Get_Markup
      do
         if Self.Lifeage = Removed then
            Insert
              (Result,
               1,
               "<span font_style=""italic"""
               & " foreground="""
               & Gtkada.Style.To_Hex (Self.Removed_Color.Get_Pref)
               & """>");
            Append (Result, "</span>");

         elsif Self.Status.Category = Not_A_Bug then
            Insert
              (Result,
               1,
               "<span foreground="""
               & Gtkada.Style.To_Hex (Self.Removed_Color.Get_Pref)
               & """>");
            Append (Result, "</span>");
         end if;
      end return;
   end Get_Markup;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Item : CWE_Category) return String is
      Image : constant String := CWE_Identifier'Image (Item.Identifier);

   begin
      return "CWE-" & Image (Image'First + 1 .. Image'Last);
   end Get_Name;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : Message_Category) return String is
   begin
      return To_String (Self.Name);
   end Get_Name;

   --------------
   -- Get_Text --
   --------------

   overriding function Get_Text
     (Self : not null access constant Message)
      return Ada.Strings.Unbounded.Unbounded_String
   is

      function Checks_Image return Unbounded_String;
      --  Return image of set of originating checks for the message.

      function CWE_Image
        (Category : Message_Category_Access) return Unbounded_String;
      --  Return image of set of CWEs of given category

      function Ranking_Image
        (Self : not null access constant Message) return String;
      --  Return an suitable Image corresponding to Message's ranking

      function Id_Image (Id : Natural) return String;
      --  Return image of the given message Id.

      ------------------
      -- Checks_Image --
      ------------------

      function Checks_Image return Unbounded_String is
         Aux : Unbounded_String;

      begin
         for Check of Self.Checks loop
            if Length (Aux) = 0 then
               Append (Aux, " (");

            else
               Append (Aux, ", ");
            end if;

            Append (Aux, Check.Name);
            Append (Aux, CWE_Image (Check));
         end loop;

         if Length (Aux) /= 0 then
            Append (Aux, ")");
         end if;

         return Aux;
      end Checks_Image;

      --------------
      -- Id_Image --
      --------------

      function Id_Image (Id : Natural) return String is
      begin
         if Self.Show_Msg_Id then
            return "[id " & String_Utils.Image (Id) & "] ";
         else
            return "";
         end if;
      end Id_Image;

      ---------------
      -- CWE_Image --
      ---------------

      function CWE_Image
        (Category : Message_Category_Access) return Unbounded_String
      is
         Aux       : Unbounded_String;
         Previous  : CWE_Identifier        := 0;
         Delimiter : Natural               := 0;
         --  Position of range delimiter.

      begin
         if Self.Display_CWEs and then not Category.CWEs.Is_Empty then
            for CWE of Category.CWEs loop
               declare
                  Image : constant String :=
                            CWE_Identifier'Image (CWE.Identifier);

               begin
                  if Length (Aux) = 0 then
                     Append (Aux, " [CWE ");
                     Append
                       (Aux, Image (Image'First + 1 .. Image'Last));
                     Delimiter := 0;

                  else
                     if Previous + 1 = CWE.Identifier then
                        --  Continuous value

                        if Delimiter = 0 then
                           Append (Aux, '-');
                           Delimiter := Length (Aux);
                           Append
                             (Aux,
                              Image (Image'First + 1 .. Image'Last));

                        else
                           Replace_Slice
                             (Aux,
                              Delimiter + 1,
                              Length (Aux),
                              Image (Image'First + 1 .. Image'Last));
                        end if;

                     else
                        Delimiter := 0;
                        Append (Aux, ',');
                        Append
                          (Aux,
                           Image (Image'First + 1 .. Image'Last));
                     end if;
                  end if;

                  Previous := CWE.Identifier;
               end;
            end loop;

            if Length (Aux) /= 0 then
               Append (Aux, ']');
            end if;

            return Aux;

         else
            return Null_Unbounded_String;
         end if;
      end CWE_Image;

      -------------------
      -- Ranking_Image --
      -------------------

      function Ranking_Image
        (Self : not null access constant Message) return String
      is
         function Decorate (S : String) return String;
         --  Append " warning" after S if Message is a warning

         function Decorate (S : String) return String is
         begin
            if Self.Is_Check then
               return S;
            else
               return S & " warning";
            end if;
         end Decorate;

      begin
         case Self.Ranking is
            when CodePeer.High =>
               return Decorate ("high");

            when CodePeer.Medium =>
               return Decorate ("medium");

            when CodePeer.Low =>
               return Decorate ("low");

            when CodePeer.Info =>
               return "info";

            when CodePeer.Suppressed =>
               return "suppressed";

            when CodePeer.Not_An_Error =>
               return "not-an-error";
         end case;
      end Ranking_Image;

   begin
      return Text : Ada.Strings.Unbounded.Unbounded_String do
         Append (Text, Id_Image (Self.Id));
         Append (Text, Ranking_Image (Self));
         Append (Text, ": ");
         Append (Text, Self.Category.Name);
         Append (Text, Checks_Image);
         Append (Text, CWE_Image (Self.Category));

         if Length (Self.Text) /= 0
           and then Element (Self.Text, 1) /= ':'
         then
            Append (Text, ' ');
         end if;

         Append (Text, Self.Text);
      end return;
   end Get_Text;

   -----------------
   -- Get_Tooltip --
   -----------------

   function Get_Tooltip (Item : CWE_Category) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Item.Name);
   end Get_Tooltip;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Code_Analysis.File_Access) return Ada.Containers.Hash_Type is
   begin
      return Item.Name.Full_Name_Hash;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Entry_Point_Information_Access) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Unbounded.Hash (Item.Name);
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Level : CodePeer.Message_Ranking_Level) return String is
   begin
      case Level is
         when CodePeer.Info =>
            return "Info";

         when CodePeer.Low =>
            return "Low";

         when CodePeer.Medium =>
            return "Medium";

         when CodePeer.High =>
            return "High";

         when CodePeer.Suppressed =>
            return "Suppressed";

         when CodePeer.Not_An_Error =>
            return "not-an-error";
      end case;
   end Image;

   function Image (Status : Audit_Status_Kinds) return String is
   begin
      return To_String (Status.Name);
   end Image;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : Annotation_Category_Access;
      Right : Annotation_Category_Access) return Boolean
   is
   begin
      return Left.Order < Right.Order;
   end Less;

   ----------
   -- Less --
   ----------

   function Less
     (Left  : CWE_Category_Access;
      Right : CWE_Category_Access) return Boolean is
   begin
      return Left.Identifier < Right.Identifier;
   end Less;

   ----------
   -- Less --
   ----------

   function Less
     (Left, Right : CodePeer.Message_Category_Access) return Boolean is
   begin
      return Left.Name < Right.Name;
   end Less;

   ----------------------
   -- Add_Audit_Status --
   ----------------------

   Next_Id : Positive := 1;

   function Add_Audit_Status
     (Status : String; Category : Audit_Status_Category)
      return Audit_Status_Kinds;
   --  Convenience function version of Add_Audit_Status

   procedure Add_Audit_Status
     (Status : String; Category : Audit_Status_Category) is
   begin
      Audit_Statuses.Append
        ((To_Unbounded_String (Status), Category, Next_Id));
      Next_Id := Next_Id + 1;
   end Add_Audit_Status;

   function Add_Audit_Status
     (Status : String; Category : Audit_Status_Category)
      return Audit_Status_Kinds
   is
      Result : constant Audit_Status_Kinds :=
        (To_Unbounded_String (Status), Category, Next_Id);
   begin
      Audit_Statuses.Append (Result);
      Next_Id := Next_Id + 1;
      return Result;
   end Add_Audit_Status;

   -----------------
   -- Standardize --
   -----------------

   function Standardize (S : String) return String is
      Result : String := To_Lower (S);
   begin
      for J in Result'Range loop
         if Result (J) = ' ' then
            Result (J) := '_';
         end if;
      end loop;

      return Result;
   end Standardize;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status (Name : String) return Audit_Status_Kinds is
   begin
      for Status of Audit_Statuses loop
         if Standardize (To_String (Status.Name)) = Standardize (Name) then
            return Status;
         end if;
      end loop;

      --  Special case "unclassified" for backward compatibility

      if To_Lower (Name) = "unclassified" then
         return Uncategorized_Status;
      end if;

      --  A new status, register and return it

      return Add_Audit_Status (Name, Not_A_Bug);
   end Get_Status;

   function Get_Status (Name : String; Category : Audit_Status_Category)
                       return Audit_Status_Kinds
   is
   begin
      for Status of Audit_Statuses loop
         if Standardize (To_String (Status.Name)) = Standardize (Name) then
            --  In case of inconsistent status category, the one already in the
            --  mapping comes from the project file so is the one to be used.
            return Status;
         end if;
      end loop;

      return Add_Audit_Status (Name, Category);
   end Get_Status;

   function Get_Status (Id : Integer) return Audit_Status_Kinds is
   begin
      for Status of Audit_Statuses loop
         if Status.Id = Id then
            return Status;
         end if;
      end loop;

      return Uncategorized_Status;
   end Get_Status;

end CodePeer;
