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

package body CodePeer.Bridge.Inspection_Readers.V6 is

   ---------------------------------
   -- Create_Inspection_Reader_V6 --
   ---------------------------------

   function Create_Inspection_Reader_V6
     (Kernel          : not null GPS.Kernel.Kernel_Handle;
      Base_Directory  : GNATCOLL.VFS.Virtual_File;
      Root_Inspection : Code_Analysis.CodePeer_Data_Access;
      Messages        : access CodePeer.Message_Maps.Map)
      return not null Inspection_Reader_Access is
   begin
      return Result : constant not null Inspection_Reader_Access :=
        new Inspection_Reader_V6 (Kernel)
      do
         declare
            Self : Inspection_Reader_V6'Class
              renames Inspection_Reader_V6'Class (Result.all);

         begin
            Base.Initialize (Self, Base_Directory, Root_Inspection, Messages);
         end;
      end return;
   end Create_Inspection_Reader_V6;

   ------------------
   -- End_Document --
   ------------------

   overriding procedure End_Document (Self : in out Inspection_Reader_V6) is

      use type Code_Analysis.Subprogram_Access;

   begin
      for Pair of Self.Postponed loop
         declare
            Subprogram_Node : constant Code_Analysis.Subprogram_Access :=
              Self.Subprogram_Map (Pair.Subprogram);
            Subprogram      : constant CodePeer.Subprogram_Data_Access :=
              (if Subprogram_Node = null then null
               else CodePeer.Subprogram_Data_Access
                 (Subprogram_Node.Analysis_Data.CodePeer_Data));

         begin
            if Subprogram = null then
               GPS.Kernel.Insert_UTF8
                 (Self.Kernel,
                  "Unable to resolve message to subprogram",
                  Mode => GPS.Kernel.Error);

            else
               --  Update name of subprogram for the message and add message to
               --  the set of subprogram's messages.

               Pair.Message.Subprogram :=
                 Ada.Strings.Unbounded.To_Unbounded_String
                   (Subprogram_Node.Name.all);
               Subprogram.Messages.Append (Pair.Message);
            end if;
         end;
      end loop;

      Self.Postponed.Clear;
   end End_Document;

   -----------------
   -- End_Message --
   -----------------

   overriding procedure End_Message (Self : in out Inspection_Reader_V6) is
   begin
      Base.Base_Inspection_Reader (Self).End_Message;
      Self.Subprogram_Node := null;
   end End_Message;

   ----------
   -- Hash --
   ----------

   function Hash
     (Item : Message_Subprogram_Pair) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item.Message.Id);
   end Hash;

   -------------------
   -- Start_Message --
   -------------------

   overriding procedure Start_Message
     (Self  : in out Inspection_Reader_V6;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      use type Code_Analysis.Subprogram_Access;

      Subprogram : constant Positive :=
        Positive'Value (Attrs.Get_Value ("subp_id"));
      Position   : constant Positive_Subprogram_Maps.Cursor :=
        Self.Subprogram_Map.Find (Subprogram);

   begin
      if Positive_Subprogram_Maps.Has_Element (Position) then
         Self.Subprogram_Node :=
           Positive_Subprogram_Maps.Element (Position);
      end if;

      Base.Base_Inspection_Reader (Self).Start_Message (Attrs);

      if Self.Subprogram_Node /= null then
         --  Append message to the list of subprogram's messages

         Self.Subprogram_Data.Messages.Append (Self.Message);

      else
         --  otherwise postpone this operation

         Self.Postponed.Insert ((Self.Message, Subprogram));
      end if;
   end Start_Message;

   ----------------------
   -- Start_Subprogram --
   ----------------------

   overriding procedure Start_Subprogram
     (Self  : in out Inspection_Reader_V6;
      Attrs : Sax.Attributes.Attributes'Class)
   is
      Subprogram_Node : Code_Analysis.Subprogram_Access;

   begin
      Subprogram_Node :=
        Code_Analysis.Get_Or_Create
          (Self.File_Node, Attrs.Get_Value ("name"));
      Subprogram_Node.Name :=
        new String'(Attrs.Get_Value ("name"));
      Subprogram_Node.Line :=
        Positive'Value (Attrs.Get_Value ("line"));
      Subprogram_Node.Column :=
        Positive'Value (Attrs.Get_Value ("column"));
      Subprogram_Node.Analysis_Data.CodePeer_Data :=
        new CodePeer.Subprogram_Data'
          (Lifeage       => Reader_Utilities.Get_Lifeage (Attrs),
           Messages      => Message_Vectors.Empty_Vector,
           Annotations   => Annotation_Maps.Empty_Map,
           Mark          => <>,
           Special_Lines => 0);
      Self.Subprogram_Map.Insert
        (Positive'Value (Attrs.Get_Value ("id")), Subprogram_Node);
   end Start_Subprogram;

   ---------------------
   -- Subprogram_Node --
   ---------------------

   overriding function Subprogram_Node
     (Self : Inspection_Reader_V6)
      return Code_Analysis.Subprogram_Access is
   begin
      return Self.Subprogram_Node;
   end Subprogram_Node;

end CodePeer.Bridge.Inspection_Readers.V6;
