------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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
with GNATdoc.Atree;           use GNATdoc.Atree;
with GNATdoc.Utils;           use GNATdoc.Utils;
with GNATdoc.Comment;         use GNATdoc.Comment;
with Language;                use Language;
with Language.Ada;
with Language.Tree;           use Language.Tree;
with Language.Tree.Database;  use Language.Tree.Database;
with String_Utils;            use String_Utils;
with Xref.Docgen;             use Xref.Docgen;

package body GNATdoc.Treepr is
   Tab : constant String :=
     "| | | | | | | | | | | | | | | | | | | | | | | | | | | | | |";

   Enhancements : constant Boolean := False;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Comments_Name
     (Basename : Filesystem_String) return Filesystem_String;
   --  Append the extension associated with comments output files

   function To_Listing_Name
     (Basename : Filesystem_String) return Filesystem_String;
   --  Append the extension associated with tree output files

   function Get_Listing_Directory
     (Kernel : Core_Kernel) return Virtual_File;
   --  Return the directory where the tree output must be generated

   function Get_Comments_Directory
     (Kernel : Core_Kernel) return Virtual_File;
   --  Return the directory where the comments output must be generated

   ----------------------------
   -- Get_Comments_Directory --
   ----------------------------

   function Get_Comments_Directory
     (Kernel : Core_Kernel) return Virtual_File
   is
      Base_Dir : constant Virtual_File := Get_Doc_Directory (Kernel);
   begin
      return Create_From_Dir (Base_Dir, +"treecm");
   end Get_Comments_Directory;

   ---------------------------
   -- Get_Listing_Directory --
   ---------------------------

   function Get_Listing_Directory
     (Kernel : Core_Kernel) return Virtual_File
   is
      Base_Dir : constant Virtual_File := Get_Doc_Directory (Kernel);
   begin
      return Create_From_Dir (Base_Dir, +"treepr");
   end Get_Listing_Directory;

   --------------------
   -- Print_Comments --
   --------------------

   procedure Print_Comments
     (Context : access constant Docgen_Context;
      Tree    : access Tree_Type)
   is
      Backend_Name  : constant String :=
                        To_String (Context.Options.Backend_Name);
      In_CM_Backend : constant Boolean := Backend_Name = "cm";
      Printout : aliased Unbounded_String;

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Print a single node

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line;

      ----------------
      -- Print_Node --
      ----------------

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result
      is
         pragma Unreferenced (Scope_Level);

         procedure Append_Structured_Comment (E : Entity_Id);
         --  Append the comment avoiding the duplicate addition of the prefix
         --  to the output

         procedure Append_Structured_Comment (E : Entity_Id) is
            Output_Mode : constant String_Mode :=
                            (if In_CM_Backend then Plain_Text_Mode
                                              else Single_Line_Mode);
         begin
            Append_Line
              (Ada.Strings.Unbounded.To_String
                 (To_Unbounded_String
                    (Comment => Get_Comment (E),
                     Prefix  => "",
                     Mode    => Output_Mode)));
         end Append_Structured_Comment;

      begin
         if Is_Skipped (Entity) then
            return OK; -- Do not output this node
         end if;

         if Context.Options.Extensions_Enabled
           and then Get_Kind (Entity) = E_Formal
         then
            null;

         --  Temporarily for disable non-required output. Done to avoid
         --  reporting false regressions.

         elsif not Enhancements then
            if Get_Kind (Entity) = E_Formal then
               return Skip;

            elsif Get_Kind (Entity) = E_Component
              and then not Is_Concurrent_Type_Or_Object (Get_Scope (Entity))
            then
               return Skip;
            end if;
         end if;

         --  No need to duplicate the output of the full view since it has
         --  been previously appended to the information of the partial view;
         --  however we return OK to output the components of the full view.

         if Is_Full_View (Entity) then
            return OK;
         end if;

         --  No output generated if no information is available on this
         --  entity (to leave the output more clean)

         if No (Get_Src (Entity))
           and then Get_Doc (Entity) = No_Comment_Result
           and then Get_Comment (Entity) = No_Structured_Comment
         then
            return OK;
         end if;

         Append_Line
           ("***** "
            & Utils.Image (LL.Get_Location (Entity))
            & ":"
            & Get_Short_Name (Entity));

         if not Context.Options.Show_Private
           or else not (Is_Partial_View (Entity))
           or else No (Get_Src (Get_Full_View (Entity)))
         then
            if Present (Get_Src (Entity)) then
               Append_Line ("--- Src:");
               Append_Line (To_String (Get_Src (Entity)));
            end if;
         else
            Append_Line ("--- Partial View Src:");
            Append_Line (To_String (Get_Src (Entity)));
            Append_Line ("--- Full View Src:");
            Append_Line (To_String (Get_Src (Get_Full_View (Entity))));
         end if;

         if Get_Doc (Entity) /= No_Comment_Result then
            Append_Line ("--- Doc.Line:" & Get_Doc (Entity).Start_Line'Img);
            Append_Line ("--- Doc.Text: " & To_String (Get_Doc (Entity).Text));
         end if;

         if Get_Comment (Entity) /= No_Structured_Comment then
            declare
               Comment : constant String :=
                 Ada.Strings.Unbounded.To_String
                   (To_Unbounded_String (Get_Comment (Entity), Prefix => ""));
            begin
               --  For backward compatibility (that is, temporarily to avoid
               --  spurious regressions) we don't avoid generating the header
               --  when we have no comment available for this entity???

               if (not Is_Record_Type (Entity)
                    and then Get_Kind (Entity) /= E_Component)
                 or else
                  (Comment /= "" and then not Spaces_Only (Comment))
               then
                  Append_Line ("--- Structured Comment:");
                  Append_Structured_Comment (Entity);
               end if;
            end;
         end if;

         if Is_Partial_View (Entity) then
            if Get_Doc (Get_Full_View (Entity)) /= No_Comment_Result then
               Append_Line
                 ("--- Full_View.Doc.Line:"
                  & Get_Doc (Get_Full_View (Entity)).Start_Line'Img);
               Append_Line
                 ("--- Full_View.Doc.Text: "
                  & To_String (Get_Doc (Get_Full_View (Entity)).Text));
            end if;

            if Context.Options.Show_Private
              and then Get_Comment (Get_Full_View (Entity))
                         /= No_Structured_Comment
            then
               Append_Line ("--- Full_View.Structured Comment:");
               Append_Structured_Comment (Get_Full_View (Entity));
            end if;
         end if;

         Append_Line ("");

         return OK;
      end Print_Node;

      --  Local variables

      Lang         : constant Language_Access :=
                       Get_Language_From_File
                        (Context.Lang_Handler, Tree.File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Root         : Entity_Id renames Tree.Tree_Root;
      Root_E       : Entity_Id;
   begin
      if In_C_Lang then
         Root_E := Root;
      else
         declare
            C : constant EInfo_List.Cursor := Get_Entities (Root).First;
         begin
            if EInfo_List.Has_Element (C) then
               Root_E := EInfo_List.Element (C);
            else
               Root_E := null;
            end if;
         end;
      end if;

      if No (Root_E) then
         return;
      end if;

      Traverse_Tree (Root_E, Print_Node'Access);

      Write_To_File
        (Context   => Context,
         Directory => Get_Comments_Directory (Context.Kernel),
         Filename  => To_Comments_Name (Tree.File.Base_Name),
         Text      => Printout'Access);
   end Print_Comments;

   ---------------------
   -- Print_Full_Tree --
   ---------------------

   procedure Print_Full_Tree
     (Context     : access constant Docgen_Context;
      Tree        : access Tree_Type;
      With_Scopes : Boolean)
   is
      Printout : aliased Unbounded_String;

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Print a single node

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Text;
      end Append_Line;

      ----------------
      -- Print_Node --
      ----------------

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result
      is
         Level : constant Natural := Scope_Level + 1;
      begin
         if Present (Get_Entities (Entity)) then
            EInfo_Vector_Sort_Loc.Sort (Get_Entities (Entity).all);
         end if;

         if With_Scopes then
            Append_Line
              (To_String
                 (E      => Entity,
                  Prefix => Tab (1 .. 2 * Level),
                  With_Errors =>
                    Context.Options.Report_Errors /= None,
                  With_Doc =>
                    Context.Options.Tree_Output.Kind /= None
                      and then Context.Options.Tree_Output.With_Comments));
         else
            Append_Line
              (To_String (Entity));
         end if;

         return OK;
      end Print_Node;

      --  Local variables

      Lang         : constant Language_Access :=
                       Get_Language_From_File
                        (Context.Lang_Handler, Tree.File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Root         : Entity_Id renames Tree.Tree_Root;
      Root_E       : Entity_Id;
   begin
      if In_C_Lang then
         Root_E := Root;
      else
         declare
            C : constant EInfo_List.Cursor := Get_Entities (Root).First;
         begin
            if EInfo_List.Has_Element (C) then
               Root_E := EInfo_List.Element (C);
            else
               Root_E := null;
            end if;
         end;
      end if;

      if No (Root_E) then
         return;
      end if;

      Traverse_Tree (Root_E, Print_Node'Access);

      Write_To_File
        (Context   => Context,
         Directory => Get_Listing_Directory (Context.Kernel),
         Filename  => To_Listing_Name (Tree.File.Base_Name),
         Text      => Printout'Access);
   end Print_Full_Tree;

   ----------------------
   -- Print_Short_Tree --
   ----------------------

   procedure Print_Short_Tree
     (Context     : access constant Docgen_Context;
      Tree        : access Tree_Type;
      With_Scopes : Boolean)
   is
      Printout : aliased Unbounded_String;

      procedure Append_Line (Text : String);
      --  Append Text to Printout plus ASCII.LF

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result;
      --  Print a single node

      -----------------
      -- Append_Line --
      -----------------

      procedure Append_Line (Text : String) is
      begin
         Printout := Printout & Text & ASCII.LF;
      end Append_Line;

      ----------------
      -- Print_Node --
      ----------------

      function Print_Node
        (Entity      : Entity_Id;
         Scope_Level : Natural) return Traverse_Result
      is
         Max_Line_Length : constant Natural := 80;

         procedure Put_Line (S : String; Level : Integer);
         procedure Put_Line (S : String; Level : Integer) is
            Last : constant Natural := Max_Line_Length - 2 * Level;
            J    : Natural := S'First;
         begin
            while J <= S'Last
              and then J - S'First <= Last
              and then S (J) /= ASCII.LF
            loop
               J := J + 1;
            end loop;

            Append_Line (Tab (1 .. 2 * Level) & S (S'First .. J - 1));

            if J <= S'Last then
               if S (J) = ASCII.LF then
                  Put_Line (S (J + 1 .. S'Last), Level);
               else
                  Put_Line (S (J .. S'Last), Level);
               end if;
            end if;
         end Put_Line;

         Text : constant String :=
                  Image (LL.Get_Location (Entity), With_Filename => False)
                      & ": " & Get_Short_Name (Entity);
         Level : constant Natural :=
                  (if With_Scopes then Scope_Level else 0);
      begin
         Put_Line
           (Text & " (" & Get_Kind (Entity)'Img & ")",
            Level);

         if Context.Options.Report_Errors /= None
           and then Present (Get_Error_Msg (Entity))
         then
            Put_Line (To_String (Get_Error_Msg (Entity)), Level);
         end if;

         if Context.Options.Tree_Output.Kind /= None
           and then Context.Options.Tree_Output.With_Comments
           and then Get_Comment (Entity) /= No_Structured_Comment
         then
            declare
               C        : Tag_Cursor := New_Cursor (Get_Comment (Entity));
               Tag_Info : Tag_Info_Ptr;
            begin
               while not At_End (C) loop
                  Tag_Info := Get (C);

                  if Present (Tag_Info.Tag) then
                     if Tag_Info.Tag = "param"
                       and then No (Tag_Info.Text)
                     then
                        null;
                     else
                        Put_Line
                          ("@"
                           & To_String (Tag_Info.Tag)
                           & " "
                           & To_String (Tag_Info.Attr),
                           Level + 2);
                     end if;
                  end if;

                  if Present (Tag_Info.Text) then
                     Put_Line
                       (Trim (Reduce (To_String (Tag_Info.Text)),
                                Ada.Strings.Left),
                        Level + 2);
                  end if;

                  Next (C);
               end loop;
            end;
         end if;

         return OK;
      end Print_Node;

      --  Local variables

      Lang         : constant Language_Access :=
                       Get_Language_From_File
                        (Context.Lang_Handler, Tree.File);
      In_Ada_Lang  : constant Boolean :=
                       Lang.all in Language.Ada.Ada_Language'Class;
      In_C_Lang    : constant Boolean := not In_Ada_Lang;
      Root         : Entity_Id renames Tree.Tree_Root;
      Root_E       : Entity_Id;
   begin
      if In_C_Lang then
         Root_E := Root;
      else
         declare
            C : constant EInfo_List.Cursor := Get_Entities (Root).First;
         begin
            if EInfo_List.Has_Element (C) then
               Root_E := EInfo_List.Element (C);
            else
               Root_E := null;
            end if;
         end;
      end if;

      if No (Root_E) then
         return;
      end if;

      Traverse_Tree (Root_E, Print_Node'Access);

      Write_To_File
        (Context   => Context,
         Directory => Get_Listing_Directory (Context.Kernel),
         Filename  => To_Listing_Name (Tree.File.Base_Name),
         Text      => Printout'Access);

   end Print_Short_Tree;

   ----------------------
   -- To_Comments_Name --
   ----------------------

   function To_Comments_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".cm";
   end To_Comments_Name;

   ---------------------
   -- To_Listing_Name --
   ---------------------

   function To_Listing_Name
     (Basename : Filesystem_String) return Filesystem_String
   is
   begin
      return Basename & ".dt";
   end To_Listing_Name;

end GNATdoc.Treepr;
