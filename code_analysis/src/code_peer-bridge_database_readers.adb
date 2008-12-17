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

with GNATCOLL.VFS;
with GPS.Kernel.Project;
with Projects.Registry;

package body Code_Peer.Bridge_Database_Readers is

   Database_Tag            : constant String := "database";
   Message_Category_Tag    : constant String := "message_category";
   Annotation_Category_Tag : constant String := "annotation_category";
   File_Tag                : constant String := "file";
   Subprogram_Tag          : constant String := "subprogram";
   Message_Tag             : constant String := "message";
   Annotation_Tag          : constant String := "annotation";

   ----------
   -- Hash --
   ----------

   function Hash (Item : Natural) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Self   : in out Reader;
      Input  : in out Input_Sources.Input_Source'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Tree   : out Code_Analysis.Code_Analysis_Tree)
   is
      Root_Project : Code_Analysis.Project_Access;

   begin
      Self.Kernel          := Kernel;
      Self.Projects        := new Code_Analysis.Project_Maps.Map;
      Self.Root_Inspection := new Code_Peer.Project_Data;
      Self.Message_Categories.Clear;
      Root_Project :=
        Code_Analysis.Get_Or_Create
          (Self.Projects,
           GPS.Kernel.Project.Get_Project (Kernel));
      Root_Project.Analysis_Data.Code_Peer_Data := Self.Root_Inspection;

      Self.Parse (Input);

      Tree := Self.Projects;
   end Parse;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Attrs         : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

      Message_Category    : Code_Peer.Message_Category_Access;
      Annotation_Category : Code_Peer.Annotation_Category_Access;
      File_Name           : GNATCOLL.VFS.Virtual_File;
      Project_Node        : Code_Analysis.Project_Access;

   begin
      if Qname = Database_Tag then
         null;

      elsif Qname = Message_Category_Tag then
         Message_Category :=
           new Code_Peer.Message_Category'
             (Name => new String'(Attrs.Get_Value ("name")));
         Code_Peer.Project_Data'Class
           (Self.Root_Inspection.all).Message_Categories.Insert
           (Message_Category);
         Self.Message_Categories.Insert
           (Natural'Value (Attrs.Get_Value ("identifier")), Message_Category);

      elsif Qname = Annotation_Category_Tag then
         Annotation_Category :=
           new Code_Peer.Annotation_Category'
             (Order => Natural'Value (Attrs.Get_Value ("identifier")),
              Text  => new String'(Attrs.Get_Value ("name")));
         Code_Peer.Project_Data'Class
           (Self.Root_Inspection.all).Annotation_Categories.Insert
           (Annotation_Category);
         Self.Annotation_Categories.Insert
           (Natural'Value (Attrs.Get_Value ("identifier")),
            Annotation_Category);

      elsif Qname = File_Tag then
         File_Name :=
           GPS.Kernel.Create (Attrs.Get_Value ("name"), Self.Kernel);
         Project_Node :=
           Code_Analysis.Get_Or_Create
             (Self.Projects, Projects.Registry.Get_Project_From_File
                  (GPS.Kernel.Project.Get_Registry (Self.Kernel).all,
                   File_Name));
         Self.File_Node :=
           Code_Analysis.Get_Or_Create (Project_Node, File_Name);

      elsif Qname = Subprogram_Tag then
         Self.Subprogram_Node :=
           Code_Analysis.Get_Or_Create
             (Self.File_Node, new String'(Attrs.Get_Value ("name")));
         Self.Subprogram_Node.Line :=
           Positive'Value (Attrs.Get_Value ("line"));
         Self.Subprogram_Node.Column :=
           Positive'Value (Attrs.Get_Value ("column"));
         Self.Subprogram_Node.Analysis_Data.Code_Peer_Data :=
           new Code_Peer.Subprogram_Data;
         Self.Subprogram_Data :=
           Code_Peer.Subprogram_Data_Access
             (Self.Subprogram_Node.Analysis_Data.Code_Peer_Data);

      elsif Qname = Message_Tag then
         Self.Subprogram_Data.Messages.Append
           (new Code_Peer.Message'
              (Positive'Value (Attrs.Get_Value ("line")),
               Positive'Value (Attrs.Get_Value ("column")),
               Self.Message_Categories.Element
                 (Positive'Value (Attrs.Get_Value ("category"))),
               Code_Peer.Message_Probability_Level'Value
                 (Attrs.Get_Value ("probability")),
               new String'(Attrs.Get_Value ("text"))));

      elsif Qname = Annotation_Tag then
         Annotation_Category :=
           Self.Annotation_Categories.Element
             (Natural'Value (Attrs.Get_Value ("category")));

         if not Self.Subprogram_Data.Annotations.Contains
                  (Annotation_Category)
         then
            Self.Subprogram_Data.Annotations.Insert
              (Annotation_Category,
               new Code_Peer.Annotation_Vectors.Vector);
         end if;

         Self.Subprogram_Data.Annotations.Element (Annotation_Category).Append
           (new Code_Peer.Annotation'
              (Text => new String'(Attrs.Get_Value ("text"))));

      else
         raise Program_Error with "Unexpected tag '" & Qname & "'";
      end if;
   end Start_Element;

end Code_Peer.Bridge_Database_Readers;
