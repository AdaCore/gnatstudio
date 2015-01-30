------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with GNATCOLL.Projects;
with GNATCOLL.Utils;
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GPS.Kernel.Project; use GPS.Kernel.Project;

with CodePeer.Module;

package body CodePeer.Bridge.Inspection_Readers is

   Inspection_Tag          : constant String := "inspection";
   Message_Category_Tag    : constant String := "message_category";
   Annotation_Category_Tag : constant String := "annotation_category";
   File_Tag                : constant String := "file";
   Subprogram_Tag          : constant String := "subprogram";
   Message_Tag             : constant String := "message";
   Annotation_Tag          : constant String := "annotation";
   Entry_Point_Tag         : constant String := "entry_point";
   Object_Race_Tag         : constant String := "object_race";
   Entry_Point_Access_Tag  : constant String := "entry_point_access";
   Object_Access_Tag       : constant String := "object_access";

   Category_Attribute       : constant String := "category";
   Checks_Attribute         : constant String := "checks";
   Column_Attribute         : constant String := "column";
   CWE_Attribute            : constant String := "cwe";
   Entry_Point_Attribute    : constant String := "entry_point";
   File_Attribute           : constant String := "file";
   Format_Attribute         : constant String := "format";
   Identifier_Attribute     : constant String := "identifier";
   Is_Check_Attribute       : constant String := "is_check";
   Kind_Attribute           : constant String := "kind";
   Line_Attribute           : constant String := "line";
   Name_Attribute           : constant String := "name";
   Previous_Attribute       : constant String := "previous";
   Primary_Checks_Attribute : constant String := "primary_checks";
   Rank_Attribute           : constant String := "rank";
   Vn_Id_Attribute          : constant String := "vn-id";
   Vn_Ids_Attribute         : constant String := "vn-ids";

   procedure Update_CWE
     (Self : in out Reader'Class; Category : Message_Category_Access);
   --  Update sets of CWEs for message category and project nodes.

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Self          : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence;
      Local_Name    : Unicode.CES.Byte_Sequence;
      Qname         : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Namespace_URI, Local_Name);

   begin
      if Self.Ignore_Depth /= 0 then
         --  Decrase depth of ignored XML element.

         Self.Ignore_Depth := Self.Ignore_Depth - 1;

      elsif Qname = Message_Tag then
         Self.Current_Message := null;

      elsif Qname = Object_Race_Tag then
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Object_Races.Append (Self.Object_Race);
         Self.Object_Race :=
           (Name         => null,
            Entry_Points => Entry_Point_Object_Access_Vectors.Empty_Vector,
            File         => GNATCOLL.VFS.No_File,
            Line         => 0,
            Column       => 0,
            Message      => null);

      elsif Qname = Entry_Point_Access_Tag then
         Self.Object_Race.Entry_Points.Append (Self.Object_Accesses);
         Self.Object_Accesses :=
           (null, CodePeer.Object_Access_Vectors.Empty_Vector);
      end if;
   end End_Element;

   ----------
   -- Hash --
   ----------

   function Hash (Item : CWE_Identifier) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Item);
   end Hash;

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
     (Self          : in out Reader;
      Input         : in out Input_Sources.Input_Source'Class;
      Kernel        : GPS.Kernel.Kernel_Handle;
      Tree          : out Code_Analysis.Code_Analysis_Tree;
      Messages      : out CodePeer.Message_Maps.Map;
      Version       : out Supported_Format_Version;
      Race_Category : out CodePeer.Message_Category_Access)
   is
      Root_Project : Code_Analysis.Project_Access;

   begin
      Self.Kernel          := Kernel;
      Self.Version         := Supported_Format_Version'First;
      Self.Ignore_Depth    := 0;
      Self.Projects        := new Code_Analysis.Project_Maps.Map;
      Self.Root_Inspection := new CodePeer.Project_Data;
      Self.Message_Categories.Clear;
      Self.Messages.Clear;
      Root_Project :=
        Code_Analysis.Get_Or_Create
          (Self.Projects,
           GPS.Kernel.Project.Get_Project (Kernel));
      Root_Project.Analysis_Data.CodePeer_Data := Self.Root_Inspection;

      Self.Parse (Input);

      Tree          := Self.Projects;
      Version       := Self.Version;
      Messages      := Self.Messages;
      Race_Category := Self.Race_Category;
   end Parse;

   --------------------
   -- Start_Document --
   --------------------

   overriding procedure Start_Document (Self : in out Reader) is
   begin
      Self.Object_Race :=
        (Name         => null,
         Entry_Points => Entry_Point_Object_Access_Vectors.Empty_Vector,
         File         => GNATCOLL.VFS.No_File,
         Line         => 0,
         Column       => 0,
         Message      => null);
   end Start_Document;

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

      Message_Category    : CodePeer.Message_Category_Access;
      Annotation_Category : CodePeer.Annotation_Category_Access;
      File_Name           : GNATCOLL.VFS.Virtual_File;
      Relocated_Name      : GNATCOLL.VFS.Virtual_File;
      Project_Node        : Code_Analysis.Project_Access;
      Entry_Point         : CodePeer.Entry_Point_Information_Access;
      Object_Access       : CodePeer.Object_Access_Information;

      function Lifeage return Lifeage_Kinds;

      function Merged return Natural_Sets.Set;
      --  Returns set of merged messages as specified in 'merged_messages'
      --  attribute. Returns empty set when attribute is not specified.

      function Checks return Natural;
      --  Returns value of "checks" attribiute if any, otherwise returns zero.

      function Is_Check return Boolean;
      --  Returns value of "is_check" attribute is any, otherwise returns
      --  False.

      function Get_Optional_Column return Positive;
      --  Returns value of "column" attribute is specified and 1 instead.

      function Get_Optional_File return GNATCOLL.VFS.Virtual_File;
      --  Returns value of "file" attribute if specified; overwise returns
      --  No_File.

      function Get_Optional_Line return Natural;
      --  Returns value of "line" attribute if specified, overwise returns 0.

      function Get_Rank return Message_Ranking_Level;
      --  Returns value of "rank" attribute. Handle old representation of Info
      --  literal ("INFORMATIONAL").

      function Get_Vns return Natural_Sets.Set;
      --  Returns value of "vn_ids" attribute if specified, overwise returns
      --  empty set.

      function Get_Vn return Natural;
      --  Returns value of "vn_id" attribute if specified, overwise returns 0.

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

      -----------------------
      -- Get_Optional_File --
      -----------------------

      function Get_Optional_File return GNATCOLL.VFS.Virtual_File is
         Index : constant Integer := Attrs.Get_Index (File_Attribute);

      begin
         if Index /= -1 then
            return
              GPS.Kernel.Create
                (+Attrs.Get_Value (File_Attribute), Self.Kernel);

         else
            return GNATCOLL.VFS.No_File;
         end if;
      end Get_Optional_File;

      -----------------------
      -- Get_Optional_Line --
      -----------------------

      function Get_Optional_Line return Natural is
         Index : constant Integer := Attrs.Get_Index (Line_Attribute);

      begin
         if Index /= -1 then
            return Natural'Value (Attrs.Get_Value (Index));

         else
            return 0;
         end if;
      end Get_Optional_Line;

      -------------------------
      -- Get_Optional_Column --
      -------------------------

      function Get_Optional_Column return Positive is
         Index : constant Integer := Attrs.Get_Index (Column_Attribute);

      begin
         if Index /= -1 then
            return Integer'Value (Attrs.Get_Value (Index));

         else
            return 1;
         end if;
      end Get_Optional_Column;

      --------------
      -- Get_Rank --
      --------------

      function Get_Rank return Message_Ranking_Level is
         Image : constant String := Attrs.Get_Value (Rank_Attribute);

      begin
         if Image = "INFORMATIONAL" then
            return Info;

         else
            return Message_Ranking_Level'Value (Image);
         end if;
      end Get_Rank;
      ------------
      -- Get_Vn --
      ------------

      function Get_Vn return Natural is
         Index : constant Integer := Attrs.Get_Index (Vn_Id_Attribute);

      begin
         if Index /= -1 then
            return Integer'Value (Attrs.Get_Value (Index));

         else
            return 0;
         end if;
      end Get_Vn;

      -------------
      -- Get_Vns --
      -------------

      function Get_Vns return Natural_Sets.Set is
         Index : constant Integer := Attrs.Get_Index (Vn_Ids_Attribute);

      begin
         if Index /= -1 then
            declare
               List   : constant GNATCOLL.Utils.Unbounded_String_Array :=
                 GNATCOLL.Utils.Split (Attrs.Get_Value (Index), ' ');
               Result : Natural_Sets.Set;

            begin
               for Item of List loop
                  Result.Insert
                    (Integer'Value (Ada.Strings.Unbounded.To_String (Item)));
               end loop;

               return Result;
            end;

         else
            return Natural_Sets.Empty_Set;
         end if;
      end Get_Vns;

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
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Current_Inspection :=
           Natural'Value (Attrs.Get_Value (Identifier_Attribute));
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Baseline_Inspection :=
           Natural'Value (Attrs.Get_Value (Previous_Attribute));
         Self.Version :=
           Format_Version'Value (Attrs.Get_Value (Format_Attribute));

      elsif Qname = Message_Category_Tag then
         Message_Category :=
           new CodePeer.Message_Category'
             (Name      => new String'(Attrs.Get_Value ("name")),
              CWE_Image =>
                (if Attrs.Get_Index (CWE_Attribute) /= -1
                 then Ada.Strings.Unbounded.To_Unbounded_String
                   (Attrs.Get_Value (CWE_Attribute))
                 else Ada.Strings.Unbounded.Null_Unbounded_String),
              CWEs      => <>);

         if Attrs.Get_Index (Is_Check_Attribute) /= -1
           and then Boolean'Value (Attrs.Get_Value (Is_Check_Attribute))
         then
            CodePeer.Project_Data'Class
              (Self.Root_Inspection.all).Check_Subcategories.Include
              (Message_Category);
         end if;

         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Message_Categories.Insert
           (Message_Category);
         Self.Message_Categories.Insert
           (Natural'Value (Attrs.Get_Value ("identifier")), Message_Category);

         Self.Update_CWE (Message_Category);

      elsif Qname = Annotation_Category_Tag then
         Annotation_Category :=
           new CodePeer.Annotation_Category'
             (Order => Natural'Value (Attrs.Get_Value ("identifier")),
              Text  => new String'(Attrs.Get_Value ("name")),
              Vn    => Get_Vn);
         CodePeer.Project_Data'Class
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

         Relocated_Name := Self.Kernel.Create_From_Base (File_Name.Base_Name);

         if Relocated_Name.Is_Regular_File then
            File_Name := Relocated_Name;
         end if;

         declare
            F_Info : constant GNATCOLL.Projects.File_Info'Class :=
              GNATCOLL.Projects.File_Info'Class
                (Get_Registry (Self.Kernel).Tree.Info_Set (File_Name)
                 .First_Element);
         begin
            Project_Node :=
              Code_Analysis.Get_Or_Create
                (Self.Projects,
                 F_Info.Project);
         end;

         Self.File_Node :=
           Code_Analysis.Get_Or_Create (Project_Node, File_Name);
         Self.File_Node.Analysis_Data.CodePeer_Data :=
           new CodePeer.File_Data'
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
         Self.Subprogram_Node.Analysis_Data.CodePeer_Data :=
           new CodePeer.Subprogram_Data'
             (Lifeage,
              Message_Vectors.Empty_Vector,
              Annotation_Maps.Empty_Map,
              null,
              0);
         Self.Subprogram_Data :=
           CodePeer.Subprogram_Data_Access
             (Self.Subprogram_Node.Analysis_Data.CodePeer_Data);

      elsif Qname = Message_Tag then
         Self.Current_Message :=
           new CodePeer.Message'
             (Positive'Value (Attrs.Get_Value ("identifier")),
              Self.File_Node,
              Self.Subprogram_Node,
              Merged,
              Lifeage,
              Positive'Value (Attrs.Get_Value ("line")),
              Positive'Value (Attrs.Get_Value ("column")),
              Self.Message_Categories.Element
                (Positive'Value (Attrs.Get_Value (Category_Attribute))),
              Is_Check,
              Get_Rank,
              Unclassified,
              True,
              new String'(Attrs.Get_Value ("text")),
              False,
              CodePeer.Audit_V3_Vectors.Empty_Vector,
              GNATCOLL.VFS.No_File,
              1,
              1,
              null,
              Message_Category_Sets.Empty_Set,
              Get_Vns);

         --  Only primary checks need to be displayed.

         if Attrs.Get_Index (Primary_Checks_Attribute) /= -1 then
            declare
               Checks : GNAT.Strings.String_List_Access :=
                 GNATCOLL.Utils.Split
                   (Attrs.Get_Value (Primary_Checks_Attribute),
                    ' ',
                    True);

            begin
               for Index in Checks'Range loop
                  Message_Category :=
                    Self.Message_Categories
                      (Natural'Value (Checks (Index).all));
                  Self.Current_Message.Checks.Insert (Message_Category);
               end loop;

               GNAT.Strings.Free (Checks);
            end;
         end if;

         if Self.Messages.Contains (Self.Current_Message.Id) then
            Self.Kernel.Insert
              (Text   =>
                 "CodePeer: duplicate message"
               & Natural'Image (Self.Current_Message.Id),
               Add_LF => False,
               Mode   => GPS.Kernel.Error);

         else
            Self.Messages.Insert
              (Self.Current_Message.Id, Self.Current_Message);
         end if;

         if Attrs.Get_Index ("from_file") /= -1 then
            Self.Current_Message.From_File :=
              GPS.Kernel.Create
                (+Attrs.Get_Value ("from_file"), Self.Kernel);
            Self.Current_Message.From_Line :=
              Positive'Value (Attrs.Get_Value ("from_line"));
            Self.Current_Message.From_Column :=
              Positive'Value (Attrs.Get_Value ("from_column"));
         end if;

         --  Append message to the list of subprogram's messages

         Self.Subprogram_Data.Messages.Append (Self.Current_Message);

         --  Append message's category to the list of corresponding
         --  categories.

         if Self.Current_Message.Is_Check then
            CodePeer.Project_Data'Class
              (Self.Root_Inspection.all).Check_Subcategories.Include
              (Self.Current_Message.Category);

         else
            CodePeer.Project_Data'Class
              (Self.Root_Inspection.all).Warning_Subcategories.Include
              (Self.Current_Message.Category);
         end if;

      elsif Qname = Annotation_Tag then
         Annotation_Category :=
           Self.Annotation_Categories.Element
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
              (Lifeage,
               new String'(Attrs.Get_Value ("text"))));

      elsif Qname = Entry_Point_Tag then
         Entry_Point :=
           new Entry_Point_Information'
             (Name   => new String'(Attrs.Get_Value (Name_Attribute)),
              File   =>
                GPS.Kernel.Create
                  (+Attrs.Get_Value (File_Attribute), Self.Kernel),
              Line   => Integer'Value (Attrs.Get_Value (Line_Attribute)),
              Column => Get_Optional_Column);
         CodePeer.Project_Data'Class
           (Self.Root_Inspection.all).Entry_Points.Insert (Entry_Point);
         Self.Entry_Point_Map.Insert
           (Integer'Value (Attrs.Get_Value ("identifier")), Entry_Point);

      elsif Qname = Object_Race_Tag then
         if Self.Race_Category = null then
            Self.Race_Category :=
              new CodePeer.Message_Category'
                (Name      =>
                    new String'(CodePeer.Module.Race_Condition_Category),
                 CWE_Image => Ada.Strings.Unbounded.Null_Unbounded_String,
                 CWEs      => CodePeer.CWE_Category_Sets.Empty_Set);
            CodePeer.Project_Data'Class
              (Self.Root_Inspection.all).Warning_Subcategories.Include
              (Self.Race_Category);
         end if;

         Self.Object_Race.Name :=
           new String'(Attrs.Get_Value (Name_Attribute));
         Self.Object_Race.File := Get_Optional_File;
         Self.Object_Race.Line := Get_Optional_Line;
         Self.Object_Race.Column := Get_Optional_Column;
         --  Object race information is added to data in the End_Element
         --  callback.

      elsif Qname = Entry_Point_Access_Tag then
         Entry_Point :=
           Self.Entry_Point_Map.Element
             (Integer'Value (Attrs.Get_Value (Entry_Point_Attribute)));
         Self.Object_Accesses :=
           (Entry_Point, CodePeer.Object_Access_Vectors.Empty_Vector);
         --  Entry point's object access information is added in the
         --  End_Element callback.

      elsif Qname = Object_Access_Tag then
         Object_Access :=
           (Kind    =>
              Object_Access_Kinds'Value (Attrs.Get_Value (Kind_Attribute)),
            File    =>
              GPS.Kernel.Create
                (+Attrs.Get_Value (File_Attribute), Self.Kernel),
            Line    => Natural'Value (Attrs.Get_Value (Line_Attribute)),
            Column  => Get_Optional_Column,
            Message => null);
         Self.Object_Accesses.Object_Accesses.Append (Object_Access);

      else
         --  Activate ignore of nested XML elements to be able to load data
         --  files of newer version then supported by GPS.

         Self.Ignore_Depth := 1;
      end if;
   end Start_Element;

   ----------------
   -- Update_CWE --
   ----------------

   procedure Update_CWE
     (Self : in out Reader'Class; Category : Message_Category_Access)
   is
      CWEs : constant String :=
        Ada.Strings.Unbounded.To_String (Category.CWE_Image);

      procedure Insert (Id : CWE_Identifier);
      --  Inserts CWE into set of CWEs

      ------------
      -- Insert --
      ------------

      procedure Insert (Id : CWE_Identifier) is
         Aux : CWE_Category_Access;

      begin
         if not Self.CWE_Categories.Contains (Id) then
            Aux := new CWE_Category'(Identifier => Id);
            Self.CWE_Categories.Include (Id, Aux);
            CodePeer.Project_Data'Class
              (Self.Root_Inspection.all).CWE_Categories.Include (Aux);
         end if;

         Category.CWEs.Include (Self.CWE_Categories (Id));
      end Insert;

      Start_CWE_Id : CWE_Identifier := 0;
      CWE_Id       : CWE_Identifier;
      First        : Positive := CWEs'First;

   begin
      for Current in CWEs'Range loop
         case CWEs (Current) is
            when '0' .. '9' =>
               null;

            when '-' =>
               Start_CWE_Id :=
                 CWE_Identifier'Value (CWEs (First .. Current - 1));
               First := Current + 1;

            when ',' =>
               CWE_Id := CWE_Identifier'Value (CWEs (First .. Current - 1));
               First := Current + 1;

               if Start_CWE_Id /= 0 then
                  --  Range starting at Start_CWE_Id + 1

                  for Id in Start_CWE_Id .. CWE_Id loop
                     Insert (Id);
                  end loop;

                  Start_CWE_Id := 0;

               else
                  Insert (CWE_Id);
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;

      if First < CWEs'Last then
         CWE_Id := CWE_Identifier'Value (CWEs (First .. CWEs'Last));

         if Start_CWE_Id /= 0 then
            --  Range starting at Start_CWE_Id + 1

            for Id in Start_CWE_Id .. CWE_Id loop
               Insert (Id);
            end loop;

            Start_CWE_Id := 0;

         else
            Insert (CWE_Id);
         end if;
      end if;
   end Update_CWE;

end CodePeer.Bridge.Inspection_Readers;
