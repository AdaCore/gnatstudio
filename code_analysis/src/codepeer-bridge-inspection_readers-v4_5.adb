------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with CodePeer.Bridge.Reader_Utilities;

package body CodePeer.Bridge.Inspection_Readers.V4_5 is

   Annotation_Tag : constant String := "annotation";

   procedure Start_Annotation
     (Self  : in out Inspection_Reader_V4_5'Class;
      Attrs : Sax.Attributes.Attributes'Class);

   -----------------------------------
   -- Create_Inspection_Reader_V4_5 --
   -----------------------------------

   function Create_Inspection_Reader_V4_5
     (Kernel          : not null GPS.Kernel.Kernel_Handle;
      Base_Directory  : GNATCOLL.VFS.Virtual_File;
      Root_Inspection : Code_Analysis.CodePeer_Data_Access;
      Messages        : access CodePeer.Message_Maps.Map)
      return not null Inspection_Reader_Access is
   begin
      return Result : constant not null Inspection_Reader_Access :=
        new Inspection_Reader_V4_5 (Kernel)
      do
         declare
            Self : Inspection_Reader_V4_5'Class
              renames Inspection_Reader_V4_5'Class (Result.all);

         begin
            Base.Initialize (Self, Base_Directory, Root_Inspection, Messages);
         end;
      end return;
   end Create_Inspection_Reader_V4_5;

   ----------------------
   -- Start_Annotation --
   ----------------------

   procedure Start_Annotation
     (Self  : in out Inspection_Reader_V4_5'Class;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Annotation_Category : CodePeer.Annotation_Category_Access;

   begin
      Annotation_Category :=
        Self.Annotation_Category
          (Natural'Value (Attrs.Get_Value ("category")));

      if not Self.Subprogram_Data.Annotations.Contains
        (Annotation_Category)
      then
         Self.Subprogram_Data.Annotations.Insert
           (Annotation_Category,
            new CodePeer.Annotation_Vectors.Vector);
      end if;

      Self.Subprogram_Data.Annotations.Element (Annotation_Category).Append
        (new CodePeer.Annotation'
           (Reader_Utilities.Get_Lifeage (Attrs),
            Reader_Utilities.Get_Value (Attrs, "text")));
   end Start_Annotation;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self  : in out Inspection_Reader_V4_5;
      Name  : String;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      if Name = Annotation_Tag then
         Self.Start_Annotation (Attrs);

      else
         Base.Base_Inspection_Reader (Self).Start_Element (Name, Attrs);
      end if;
   end Start_Element;

   -------------------
   -- Start_Message --
   -------------------

   overriding procedure Start_Message
     (Self  : in out Inspection_Reader_V4_5;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      Base.Base_Inspection_Reader (Self).Start_Message (Attrs);
      --  Call base implementation.

      --  Append message to the list of subprogram's messages

      Self.Subprogram_Data.Messages.Append (Self.Message);
   end Start_Message;

   ----------------------
   -- Start_Subprogram --
   ----------------------

   overriding procedure Start_Subprogram
     (Self  : in out Inspection_Reader_V4_5;
      Attrs : Sax.Attributes.Attributes'Class) is
   begin
      Self.Subprogram_Node :=
        Code_Analysis.Get_Or_Create
          (Self.File_Node, Attrs.Get_Value ("name"));
      Self.Subprogram_Node.Name :=
        new String'(Attrs.Get_Value ("name"));
      Self.Subprogram_Node.Line :=
        Positive'Value (Attrs.Get_Value ("line"));
      Self.Subprogram_Node.Column :=
        Positive'Value (Attrs.Get_Value ("column"));
      Self.Subprogram_Node.Analysis_Data.CodePeer_Data :=
        new CodePeer.Subprogram_Data'
          (Lifeage       => Reader_Utilities.Get_Lifeage (Attrs),
           Messages      => Message_Vectors.Empty_Vector,
           Annotations   => Annotation_Maps.Empty_Map,
           Mark          => <>,
           Special_Lines => 0);
   end Start_Subprogram;

   ---------------------
   -- Subprogram_Node --
   ---------------------

   overriding function Subprogram_Node
     (Self : Inspection_Reader_V4_5)
      return Code_Analysis.Subprogram_Access is
   begin
      return Self.Subprogram_Node;
   end Subprogram_Node;

end CodePeer.Bridge.Inspection_Readers.V4_5;
