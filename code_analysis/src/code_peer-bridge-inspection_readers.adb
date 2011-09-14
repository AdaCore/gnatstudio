-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2011, AdaCore                 --
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

with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GPS.Kernel.Project; use GPS.Kernel.Project;

package body Code_Peer.Bridge.Inspection_Readers is

   Inspection_Tag          : constant String := "inspection";
   Message_Category_Tag    : constant String := "message_category";
   Annotation_Category_Tag : constant String := "annotation_category";
   File_Tag                : constant String := "file";
   Subprogram_Tag          : constant String := "subprogram";
   Message_Tag             : constant String := "message";
   Annotation_Tag          : constant String := "annotation";

   Checks_Attribute        : constant String := "checks";
   Format_Attribute        : constant String := "format";
   Identifier_Attribute    : constant String := "identifier";
   Is_Check_Attribute      : constant String := "is_check";
   Is_Warning_Attribute    : constant String := "is_warning";
   Previous_Attribute      : constant String := "previous";

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name, Qname);

   begin
      if Self.Ignore_Depth /= 0 then
         --  Decrase depth of ignored XML element.

         Self.Ignore_Depth := Self.Ignore_Depth - 1;
      end if;
   end End_Element;

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
      Self.Version         := 1;
      Self.Ignore_Depth    := 0;
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
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence;
      Attrs         : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

      Message_Category    : Code_Peer.Message_Category_Access;
      Annotation_Category : Code_Peer.Annotation_Category_Access;
      File_Name           : GNATCOLL.VFS.Virtual_File;
      Relocated_Name      : GNATCOLL.VFS.Virtual_File;
      Project_Node        : Code_Analysis.Project_Access;

      function Lifeage return Lifeage_Kinds;

      function Merged return Natural_Sets.Set;
      --  Returns set of merged messages as specified in 'merged_messages'
      --  attribute. Returns empty set when attribute is not specified.

      function Computed_Ranking return Message_Ranking_Level;
      --  Returns value of "computed_probability" attribute if any, otherwise
      --  returns value of "probability" attribute.

      function Checks return Natural;
      --  Returns value of "checks" attribiute if any, otherwise returns zero.

      function Is_Check return Boolean;
      --  Returns value of "is_check" attribute is any, otherwise returns
      --  False.

      function Is_Warning return Boolean;
      --  Returns value of "is_warning" attribute is any, otherwise returns
      --  False.

      ------------
      -- Checks --
      ------------

      function Checks return Natural is
         Index : constant Integer := Attrs.Get_Index (Checks_Attribute);

      begin
         if Index = -1 then
            return 0;

         else
            return Natural'Value (Attrs.Get_Value (Index));
         end if;
      end Checks;

      ----------------------
      -- Computed_Ranking --
      ----------------------

      function Computed_Ranking return Message_Ranking_Level is
         Index : constant Integer := Attrs.Get_Index ("computed_probability");

      begin
         if Index = -1 then
            return
              Message_Ranking_Level'Value (Attrs.Get_Value ("probability"));

         else
            return Message_Ranking_Level'Value (Attrs.Get_Value (Index));
         end if;
      end Computed_Ranking;

      --------------
      -- Is_Check --
      --------------

      function Is_Check return Boolean is
         Index : constant Integer := Attrs.Get_Index (Is_Check_Attribute);

      begin
         if Index = -1 then
            return False;

         else
            return Boolean'Value (Attrs.Get_Value (Index));
         end if;
      end Is_Check;

      ----------------
      -- Is_Warning --
      ----------------

      function Is_Warning return Boolean is
         Index : constant Integer := Attrs.Get_Index (Is_Warning_Attribute);

      begin
         if Index = -1 then
            return False;

         else
            return Boolean'Value (Attrs.Get_Value (Index));
         end if;
      end Is_Warning;

      -------------
      -- Lifeage --
      -------------

      function Lifeage return Lifeage_Kinds is
         Index : constant Integer := Attrs.Get_Index ("lifeage");

      begin
         if Index = -1 then
            return Unchanged;

         else
            return Lifeage_Kinds'Value (Attrs.Get_Value (Index));
         end if;
      end Lifeage;

      ------------
      -- Merged --
      ------------

      function Merged return Natural_Sets.Set is
         Index  : constant Integer := Attrs.Get_Index ("merged_messages");
         Result : Natural_Sets.Set;

      begin
         if Index /= -1 then
            declare
               Value : constant String := Attrs.Get_Value (Index);
               First : Positive := Value'First;
               Last  : Positive := Value'First;

            begin
               loop
                  --  Skip spaces

                  while Last <= Value'Last loop
                     First := Last;

                     exit when Value (Last) /= ' ';

                     Last := Last + 1;
                  end loop;

                  --  Looking for identifier

                  while Last <= Value'Last loop
                     exit when Value (Last) = ' ';

                     Last := Last + 1;
                  end loop;

                  if First <= Value'Last and First < Last then
                     Result.Insert (Natural'Value (Value (First .. Last - 1)));
                  end if;

                  exit when Last > Value'Last;
               end loop;
            end;
         end if;

         return Result;
      end Merged;

   begin
      if Self.Ignore_Depth /= 0 then
         Self.Ignore_Depth := Self.Ignore_Depth + 1;

      elsif Qname = Inspection_Tag then
         Code_Peer.Project_Data'Class
           (Self.Root_Inspection.all).Current_Inspection :=
           Natural'Value (Attrs.Get_Value (Identifier_Attribute));
         Code_Peer.Project_Data'Class
           (Self.Root_Inspection.all).Baseline_Inspection :=
           Natural'Value (Attrs.Get_Value (Previous_Attribute));

         if Attrs.Get_Index (Format_Attribute) /= -1 then
            Self.Version :=
              Positive'Value (Attrs.Get_Value (Format_Attribute));
         end if;

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
           GPS.Kernel.Create (+Attrs.Get_Value ("name"), Self.Kernel);
         --  ??? Potentially non-utf8 string should not be
         --  stored in an XML attribute.

         --  Try to find file with the same base name in the current project.
         --  Use this file instead of original name which comes from the
         --  database to be able to reuse database between several users.

         Relocated_Name :=
           GPS.Kernel.Create_From_Base (File_Name.Base_Name, Self.Kernel);

         if Relocated_Name.Is_Regular_File then
            File_Name := Relocated_Name;
         end if;

         Project_Node :=
           Code_Analysis.Get_Or_Create
             (Self.Projects,
              Get_Registry (Self.Kernel).Tree.Info (File_Name).Project);
         Self.File_Node :=
           Code_Analysis.Get_Or_Create (Project_Node, File_Name);
         Self.File_Node.Analysis_Data.Code_Peer_Data :=
           new Code_Peer.File_Data'
                 (Lifeage      => Lifeage,
                  Total_Checks => Checks);

      elsif Qname = Subprogram_Tag then
         Self.Subprogram_Node :=
           Code_Analysis.Get_Or_Create
             (Self.File_Node, new String'(Attrs.Get_Value ("name")));
         Self.Subprogram_Node.Line :=
           Positive'Value (Attrs.Get_Value ("line"));
         Self.Subprogram_Node.Column :=
           Positive'Value (Attrs.Get_Value ("column"));
         Self.Subprogram_Node.Analysis_Data.Code_Peer_Data :=
           new Code_Peer.Subprogram_Data'
             (Lifeage,
              Message_Vectors.Empty_Vector,
              Annotation_Maps.Empty_Map,
              null,
              0);
         Self.Subprogram_Data :=
           Code_Peer.Subprogram_Data_Access
             (Self.Subprogram_Node.Analysis_Data.Code_Peer_Data);

      elsif Qname = Message_Tag then
         declare
            Message : Code_Peer.Message_Access;

         begin
            Message :=
              new Code_Peer.Message'
                (Positive'Value (Attrs.Get_Value ("identifier")),
                 Merged,
                 Lifeage,
                 Positive'Value (Attrs.Get_Value ("line")),
                 Positive'Value (Attrs.Get_Value ("column")),
                 Self.Message_Categories.Element
                   (Positive'Value (Attrs.Get_Value ("category"))),
                 Is_Warning,
                 Is_Check,
                 Computed_Ranking,
                 Code_Peer.Message_Ranking_Level'Value
                   (Attrs.Get_Value ("probability")),
                 new String'(Attrs.Get_Value ("text")),
                 False,
                 Code_Peer.Audit_Vectors.Empty_Vector,
                 GNATCOLL.VFS.No_File,
                 1,
                 1,
                 null);

            if Attrs.Get_Index ("from_file") /= -1 then
               Message.From_File :=
                 GPS.Kernel.Create
                   (+Attrs.Get_Value ("from_file"), Self.Kernel);
               Message.From_Line :=
                 Positive'Value (Attrs.Get_Value ("from_line"));
               Message.From_Column :=
                 Positive'Value (Attrs.Get_Value ("from_column"));
            end if;

            if Self.Version = 1 then
               --  Use heuristic to compute value of 'is_warning' attribute.
               --  This code is used for CodePeer 2.0 and can be removed in
               --  the future.
               --  ??? Should be reviewed after begining of 2013 and be
               --  removed.

               declare

                  function Starts_With
                    (Item : String; Prefix : String) return Boolean;
                  --  Returns True when Item starts with Prefix.

                  -----------------
                  -- Starts_With --
                  -----------------

                  function Starts_With
                    (Item : String; Prefix : String) return Boolean
                  is
                     Last : constant Natural := Item'First + Prefix'Length - 1;

                  begin
                     return Item'Length >= Prefix'Length
                       and then Item (Item'First .. Last) = Prefix;
                  end Starts_With;

                  Category : constant String := Message.Category.Name.all;

               begin
                  Message.Is_Warning :=
                    Category = "dead code"
                      or else Starts_With (Category, "mismatched ")
                      or else Starts_With (Category, "suspicious ")
                      or else Starts_With (Category, "test ")
                      or else Starts_With (Category, "unused ")
                      or else Starts_With (Category, "unprotected ");
               end;
            end if;

            --  Append message to the list of subprogram's messages

            Self.Subprogram_Data.Messages.Append (Message);

            --  Append message's category to the list of corresponding
            --  categories.

            if Message.Is_Warning then
               Code_Peer.Project_Data'Class
                 (Self.Root_Inspection.all).Warning_Categories.Include
                 (Message.Category);

            elsif Message.Is_Check then
               Code_Peer.Project_Data'Class
                 (Self.Root_Inspection.all).Check_Categories.Include
                 (Message.Category);

            else
               Code_Peer.Project_Data'Class
                 (Self.Root_Inspection.all).General_Categories.Include
                 (Message.Category);
            end if;
         end;

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
              (Lifeage,
               new String'(Attrs.Get_Value ("text"))));

      else
         --  Activate ignore of nested XML elements to be able to load data
         --  files of newer version then supported by GPS.

         Self.Ignore_Depth := 1;
      end if;
   end Start_Element;

end Code_Peer.Bridge.Inspection_Readers;
