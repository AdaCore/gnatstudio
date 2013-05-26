------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNATCOLL.Projects;       use GNATCOLL.Projects;
with GPS.Kernel.Project;      use GPS.Kernel.Project;
with Docgen3.Comment;         use Docgen3.Comment;
with Docgen3.Time;            use Docgen3.Time;
with Docgen3.Utils;           use Docgen3.Utils;
with Language;                use Language;
with Language.Ada;            use Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with Templates_Parser;        use Templates_Parser;
with Traces;                  use Traces;

package body Docgen3.Backend is
   Me : constant Debug_Handle := Create ("Docgen3.1-Backend");

   type Template_Kind is
     (Tmpl_Entities,
      --  Public entities documentation
      Tmpl_Index,
      --  Packages, source files and entities Index
      Tmpl_Src
      --  Source code
     );

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Append_List_ReST
     (Context        : access constant Docgen_Context;
      Printout       : in out Unbounded_String;
      List           : in out EInfo_List.Vector;
      Header         : String;
      With_Full_Name : Boolean := False);
   --  Append to Printout the reStructured Text for List

   procedure Clear (Info : access Collected_Entities);
   --  Clear all the lists used to classify the tree nodes in categories

   function Get_Template
     (System_Dir : Virtual_File;
      Kind       : Template_Kind) return Virtual_File;

   function To_Destination_Name
     (Basename : Filesystem_String) return Filesystem_String;

   ----------------------
   -- Append_List_ReST --
   ----------------------

   procedure Append_List_ReST
     (Context        : access constant Docgen_Context;
      Printout       : in out Unbounded_String;
      List           : in out EInfo_List.Vector;
      Header         : String;
      With_Full_Name : Boolean := False)
   is
      function Get_Name (E : Entity_Id) return String;
      function Get_Name (E : Entity_Id) return String is
      begin
         if With_Full_Name then
            return Get_Full_Name (E);
         else
            return Get_Short_Name (E);
         end if;
      end Get_Name;

      --  Local variables

      Cursor : EInfo_List.Cursor;
      Decl   : General_Entity_Declaration;
      E      : General_Entity;
      E_Info : Entity_Id;
      Loc    : General_Location;

   --  Start of processing for Append_List

   begin
      if not EInfo_List.Has_Element (List.First) then
         return;
      end if;

      EInfo_Vector_Sort_Full.Sort (List);

      Printout :=
        Printout
          & "- " & Header & ASCII.LF;

      Cursor := List.First;
      while EInfo_List.Has_Element (Cursor) loop
         E_Info := EInfo_List.Element (Cursor);

         E    := LL.Get_Entity (E_Info);
         Decl := Get_Declaration (Context.Database, E);
         Loc  := Decl.Loc;

         Printout :=
           Printout
             & "   * :ref:`" & Get_Name (E_Info) & "` "
             & Image (Loc)
             & ASCII.LF;

         EInfo_List.Next (Cursor);
      end loop;

      Printout :=
        Printout
        & ASCII.LF;
   end Append_List_ReST;

   -----------
   -- Clear --
   -----------

   procedure Clear (Info : access Collected_Entities) is
   begin
      Info.Pkgs.Clear;
      Info.Variables.Clear;
      Info.Types.Clear;
      Info.Record_Types.Clear;
      Info.Subprgs.Clear;
      Info.Tagged_Types.Clear;
      Info.Methods.Clear;
   end Clear;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Context             : access constant Docgen_Context;
      Src_Files           : in out Files_List.Vector;
      Info                : Backend_Info;
      Update_Global_Index : Boolean)
   is
      procedure Generate_Global_Index
        (Context   : access constant Docgen_Context;
         Src_Files : in out Files_List.Vector;
         Info      : Backend_Info);

      ---------------------------
      -- Generate_Global_Index --
      ---------------------------

      procedure Generate_Global_Index
        (Context   : access constant Docgen_Context;
         Src_Files : in out Files_List.Vector;
         Info      : Backend_Info)
      is
         procedure Generate_Entities
           (Context : access constant Docgen_Context);

         -----------------------
         -- Generate_Entities --
         -----------------------

         procedure Generate_Entities
           (Context : access constant Docgen_Context)
         is
            Printout    : Unbounded_String;
            Translation : Translate_Set;
            Tmpl        : constant Virtual_File :=
                            Get_Template
                             (Get_System_Dir (Context.Kernel), Tmpl_Entities);

         begin
            Printout :=
              Printout
              & "All entities" & ASCII.LF
              & "============" & ASCII.LF
              & ASCII.LF;

            Append_List_ReST
              (Context, Printout, Info.Pkgs, "Packages",
               With_Full_Name => True);
            Append_List_ReST
              (Context, Printout, Info.Types, "Types");
            Append_List_ReST
              (Context, Printout, Info.Record_Types, "Records");
            Append_List_ReST
              (Context, Printout, Info.Subprgs, "Subprograms");
            Append_List_ReST
              (Context, Printout, Info.Tagged_Types, "Tagged types");
            Append_List_ReST
              (Context, Printout, Info.Methods, "Methods");

            Insert
              (Translation, Assoc ("PRINTOUT", Printout));

            Write_To_File
              (Context   => Context,
               Directory => Get_Doc_Directory (Context.Kernel),
               Filename  => To_Destination_Name ("entities"),
               Text =>
                 Parse (+Tmpl.Full_Name, Translation, Cached => True));
         end Generate_Entities;

         --  Local variables

         Printout    : Unbounded_String;
         Translation : Translate_Set;
         Tmpl        : constant Virtual_File :=
                         Get_Template
                           (Get_System_Dir (Context.Kernel), Tmpl_Index);
         File        : GNATCOLL.VFS.Virtual_File;
         File_Index  : Files_List.Cursor;
         My_Delay    : Delay_Time;

      --  Start of processing for Generate_Global_Index

      begin
         Start (My_Delay);
         Trace (Me, "Generate_Global_Index");

         Generate_Entities (Context);

         Files_Vector_Sort.Sort (Src_Files);

         File_Index := Src_Files.First;
         while Files_List.Has_Element (File_Index) loop
            File := Files_List.Element (File_Index);

            if Is_Spec_File (Context.Kernel, File) then
               Printout :=
                 Printout
                 & "   " & (+File.Base_Name) & ASCII.LF;
            end if;

            Files_List.Next (File_Index);
         end loop;

         Insert (Translation, Assoc ("PRINTOUT", Printout));

         Write_To_File
           (Context   => Context,
            Directory => Get_Doc_Directory (Context.Kernel),
            Filename  => To_Destination_Name ("index"),
            Text =>
              Parse (+Tmpl.Full_Name, Translation, Cached => True));

         Stop (My_Delay, Generate_Global_Index_Time);
      end Generate_Global_Index;

   --  Start of processing for Finalize

   begin
      if Update_Global_Index then
         Generate_Global_Index (Context, Src_Files, Info);
      end if;

      Clear (Info);
   end Finalize;

   -----------------------
   -- Get_Doc_Directory --
   -----------------------

   function Get_Doc_Directory
     (Kernel : Kernel_Handle) return Virtual_File
   is
      Base_Dir : Virtual_File;
      Project  : Project_Type renames
                   Get_Registry (Kernel).Tree.Root_Project;
      Attr     : constant String :=
                   Project.Attribute_Value (Documentation_Dir_Attribute);

   begin
      if Attr /= "" then
         Base_Dir := Create_From_Base (+Attr);
         Base_Dir.Ensure_Directory;

         return Base_Dir;
      end if;

      if Project.Object_Dir /= No_File then
         Base_Dir := Project.Object_Dir;
      else
         Base_Dir := Project.Project_Path.Get_Parent;
      end if;

      return Create_From_Dir (Base_Dir, +"doc/");
   end Get_Doc_Directory;

   ------------------
   -- Get_Template --
   ------------------

   function Get_Template
     (System_Dir : Virtual_File;
      Kind       : Template_Kind) return Virtual_File is
   begin
      case Kind is
         when Tmpl_Entities =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen3/entities.tmpl");
         when Tmpl_Index =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen3/index.tmpl");
         when Tmpl_Src =>
            return Create_From_Dir
              (System_Dir, "share/gps/docgen3/src.tmpl");
      end case;
   end Get_Template;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Context : access constant Docgen_Context;
      Info    : out Backend_Info)
   is
      procedure Generate_Support_Files
        (Kernel : Kernel_Handle);
      --  Generate support files in destination directory

      ----------------------------
      -- Generate_Support_Files --
      ----------------------------

      procedure Generate_Support_Files
        (Kernel : Kernel_Handle)
      is
         Src_Dir : constant GNATCOLL.VFS.Virtual_File :=
                     Create_From_Dir
                      (Get_System_Dir (Kernel), "share/gps/docgen3/support/");
         Dst_Dir : constant GNATCOLL.VFS.Virtual_File :=
                     Get_Doc_Directory (Kernel);
         Success : Boolean;

      begin
         if not Is_Directory (Dst_Dir) then
            Dst_Dir.Make_Dir;
         end if;

         --  Copy the "support" directory into the target directory

         Src_Dir.Copy (Dst_Dir.Full_Name, Success);
         pragma Assert (Success);
      end Generate_Support_Files;

   begin
      Info := new Collected_Entities;
      Generate_Support_Files (Context.Kernel);
   end Initialize;

   ------------------
   -- Process_File --
   ------------------

   procedure Process_File
     (Context : access constant Docgen_Context;
      Tree    : access Tree_Type;
      Info    : Backend_Info)
   is
      Printout     : Unbounded_String;
      Translation  : Translate_Set;
      Tmpl         : constant Virtual_File :=
                       Get_Template
                         (Get_System_Dir (Context.Kernel), Tmpl_Src);
      File_Entities : aliased Collected_Entities;
      use Comment;

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      procedure Append_Subprogram_ReST (E : Entity_Id);
      --  Append to Printout the reStructured output of a type

      procedure Append_Type_ReST (E : Entity_Id);
      --  Append to Printout the reStructured output of a type

      procedure Classify_Entity (E : Entity_Id);
      --  Classify the entity in one of the following categories: Method,
      --  subprogram, tagged type, record type, type, variable or package.

      function To_ReST
        (Comment : Structured_Comment) return Unbounded_String;
      --  Convert the structured comment to the reStructured output

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line;

      ----------------------------
      -- Append_Subprogram_ReST --
      ----------------------------

      procedure Append_Subprogram_ReST (E : Entity_Id) is
         Xref_E : constant General_Entity := LL.Get_Entity (E);
         Name   : constant String := Get_Name (Context.Database, Xref_E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         Append_Line (".. _" & Name & ":"); --  Label
         Append_Line ("");
         Append_Line (Name);
         Append_Line (Header);              -- Header
         Append_Line ("");

         if Get_Src (E) /= Null_Unbounded_String then
            Append_Line ("**profile**");
            Append_Line ("");
            Append_Line ("::");
            Append_Line ("");
            Append_Line (To_String (Get_Src (E)));
         end if;

         declare
            Comment_Str : constant String :=
                            To_String (To_ReST (Get_Comment (E)));
         begin
            if Comment_Str /= "" then
               Append_Line ("");
               Append_Line (Comment_Str);
            end if;
         end;
      end Append_Subprogram_ReST;

      ----------------------
      -- Append_Type_ReST --
      ----------------------

      procedure Append_Type_ReST (E : Entity_Id) is
         LL_E   : constant General_Entity := LL.Get_Entity (E);
         Name   : constant String := Get_Name (Context.Database, LL_E);
         Header : constant String (Name'Range) := (others => '=');

      begin
         Append_Line (".. _" & Name & ":"); --  Label
         Append_Line ("");
         Append_Line (Name);
         Append_Line (Header);              -- Header
         Append_Line ("");

         if Get_Src (E) /= Null_Unbounded_String then
            Append_Line ("**type**");
            Append_Line ("");
            Append_Line ("::");
            Append_Line ("");
            Append_Line (To_String (Get_Src (E)));
            Append_Line ("");
         end if;

         declare
            Comment_Str : constant String :=
                            To_String (To_ReST (Get_Comment (E)));
         begin
            if Comment_Str /= "" then
               Append_Line ("");
               Append_Line (Comment_Str);
            end if;
         end;
      end Append_Type_ReST;

      ---------------------
      -- Classify_Entity --
      ---------------------

      procedure Classify_Entity (E : Entity_Id) is
      begin
         if LL.Is_Subprogram (E) then
            if not LL.Is_Primitive (E) then
               File_Entities.Subprgs.Append (E);
               Info.Subprgs.Append (E);
            else
               File_Entities.Methods.Append (E);
               Info.Methods.Append (E);
            end if;

         elsif Is_Tagged (E) then
            File_Entities.Tagged_Types.Append (E);
            Info.Tagged_Types.Append (E);

         elsif Is_Record_Type (E) then
            File_Entities.Record_Types.Append (E);
            Info.Record_Types.Append (E);

         elsif LL.Is_Type (E) then
            File_Entities.Types.Append (E);
            Info.Types.Append (E);

         elsif Get_Kind (E) = E_Variable then
            File_Entities.Variables.Append (E);
            Info.Variables.Append (E);

         elsif Is_Package (E) then
            File_Entities.Pkgs.Append (E);
            Info.Pkgs.Append (E);
         end if;
      end Classify_Entity;

      -------------
      -- To_ReST --
      -------------

      function To_ReST
        (Comment : Structured_Comment) return Unbounded_String
      is
         Result : Unbounded_String := Null_Unbounded_String;

         procedure Add (Tag_Info : Tag_Info_Ptr);
         procedure Add (Tag_Info : Tag_Info_Ptr) is
         begin
            if Tag_Info.Tag /= Null_Unbounded_String then
               if Tag_Info.Tag = "seealso" then
                  Result := Result
                    & "**" & Tag_Info.Tag & "**" & ASCII.LF
                    & " :ref:`" & Tag_Info.Attr & "`" & ASCII.LF
                    & ASCII.LF;

               else
                  Result := Result
                    & "**" & Tag_Info.Tag & "** "
                    & Tag_Info.Attr & ASCII.LF
                    & ASCII.LF
                    & Trim (Reduce (To_String (Tag_Info.Text)),
                            Ada.Strings.Left) & ASCII.LF
                    & ASCII.LF;
               end if;
            end if;
         end Add;

         C        : Tag_Cursor := New_Cursor (Comment);
         Tag_Info : Tag_Info_Ptr;
      begin
         if not At_End (C) then
            --  General description (if any)

            Tag_Info := Get (C);
            Add (Tag_Info);
            Next (C);

            --  Tags

            while not At_End (C) loop
               Tag_Info := Get (C);
               Add (Tag_Info);
               Next (C);
            end loop;
         end if;

         return Result;
      end To_ReST;

      --  Local variables

      Db           : General_Xref_Database renames Context.Database;
      Lang         : constant Language_Access :=
                       Get_Language_From_File
                         (Context.Lang_Handler, Tree.File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Root_Id      : Entity_Id renames Tree.Tree_Root;
      Current_Unit : Entity_Id;

      My_Delay     : Delay_Time;

   --  Start of processing for Process_File

   begin
      Trace (Me, "Process_File " & (+Tree.File.Base_Name));

      if No (Root_Id) then
         return;

      --  C & CPP are not supported yet (may work but they have not been
      --  tested yet!)

      elsif In_C_Lang then
         return;
      end if;

      Start (My_Delay);

      Current_Unit := Get_Entities (Tree.Tree_Root).First_Element;

      --  Classify the tree nodes in categories

      For_All (Tree.All_Entities, Classify_Entity'Access);

      Append_Line ("Entities");
      Append_Line ("========");
      Append_Line ("");

      Append_List_ReST
        (Context, Printout, File_Entities.Types, "Types");
      Append_List_ReST
        (Context, Printout, File_Entities.Record_Types, "Records");
      Append_List_ReST
        (Context, Printout, File_Entities.Subprgs, "Subprograms");
      Append_List_ReST
        (Context, Printout, File_Entities.Tagged_Types, "Tagged types");
      Append_List_ReST
        (Context, Printout, File_Entities.Methods, "Methods");

      --  Generate full documentation

      For_All (File_Entities.Types,        Append_Type_ReST'Access);
      For_All (File_Entities.Record_Types, Append_Type_ReST'Access);
      For_All (File_Entities.Tagged_Types, Append_Type_ReST'Access);

      For_All (File_Entities.Subprgs, Append_Subprogram_ReST'Access);
      For_All (File_Entities.Methods, Append_Subprogram_ReST'Access);

      declare
         E_Pkg    : constant General_Entity := LL.Get_Entity (Current_Unit);
         Pkg_Name : constant String := Get_Name (Db, E_Pkg);
         Filename : constant String := +Tree.File.Base_Name;
         Header   : constant String (Filename'Range) := (others => '*');
      begin
         Printout :=
           ".. _" & Pkg_Name & ":" & ASCII.LF & ASCII.LF      --  Label
           & Header   & ASCII.LF
           & Filename & ASCII.LF
           & Header   & ASCII.LF
           & ASCII.LF & Printout & ASCII.LF;
      end;

      Insert (Translation, Assoc ("PRINTOUT", Printout));

      Write_To_File
        (Context   => Context,
         Directory => Get_Doc_Directory (Context.Kernel),
         Filename  => To_Destination_Name (Tree.File.Base_Name),
         Text =>
           Parse (+Tmpl.Full_Name, Translation, Cached => True));

      Clear (File_Entities'Access);

      Stop (My_Delay, Generate_Doc_Time);
   end Process_File;

   -------------------------
   -- To_Destination_Name --
   -------------------------

   function To_Destination_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".rst";
   end To_Destination_Name;

end Docgen3.Backend;
