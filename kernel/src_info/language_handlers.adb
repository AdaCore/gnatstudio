------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with GNAT.Bubble_Sort_G;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Projects;

with Case_Handling;             use Case_Handling;
with GPS.Properties;            use GPS.Properties;
with Language.Unknown;          use Language.Unknown;
with Language;                  use Language;
with Language.Tree;             use Language.Tree;
with Language.Tree.Database;    use Language.Tree.Database;
with Projects;                  use Projects;
with GNATCOLL.Traces;                    use GNATCOLL.Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

package body Language_Handlers is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.LANGUAGE_HANDLERS");

   function Get_Index_From_Language
     (Handler       : access Language_Handler_Record'Class;
      Language_Name : String) return Natural;
   --  Return the index of Language in Handler.Languages, or 0 if no such
   --  language is known.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Language_Info_Array, Language_Info_Access);

   --------------------
   -- Create_Handler --
   --------------------

   procedure Create_Handler
     (Handler : out Language_Handler;
      Symbols : not null access GNATCOLL.Symbols.Symbol_Table_Record'Class) is
   begin
      --  ??? Never freed, but the handler is never destroyed
      Handler := new Language_Handler_Record;
      Handler.Symbols := GNATCOLL.Symbols.Symbol_Table_Access (Symbols);
   end Create_Handler;

   ------------------
   -- Set_Registry --
   ------------------

   procedure Set_Registry
     (Handler  : access Language_Handler_Record;
      Registry : access Project_Registry'Class) is
   begin
      Handler.Registry := Project_Registry_Access (Registry);
   end Set_Registry;

   -----------------------------
   -- Get_Index_From_Language --
   -----------------------------

   function Get_Index_From_Language
     (Handler       : access Language_Handler_Record'Class;
      Language_Name : String) return Natural
   is
      Lang : constant String := To_Lower (Language_Name);
   begin
      Assert (Me, Handler.Languages /= null, "No registered language");

      for Index in Handler.Languages'Range loop
         if To_Lower (Get_Name (Handler.Languages (Index).Lang)) = Lang then
            return Index;
         end if;
      end loop;
      return 0;
   end Get_Index_From_Language;

   ----------------------------
   -- Language_Is_Overridden --
   ----------------------------

   function Language_Is_Overridden
     (Handler  : access Language_Handler_Record;
      Filename : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      pragma Unreferenced (Handler);
      Prop  : String_Property;
      Found : Boolean := False;
   begin
      Get_Property (Prop, Filename, "language", Found);
      return Found;
   end Language_Is_Overridden;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   overriding function Get_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return Language.Language_Access
   is
      Index : Natural;
   begin
      Index := Get_Index_From_Language
        (Handler,
         Get_Language_From_File (Handler, Source_Filename, From_Project_Only));
      if Index /= 0 then
         return Handler.Languages (Index).Lang;
      end if;

      return Unknown_Lang;
   end Get_Language_From_File;

   ---------------------------------
   -- Get_Tree_Language_From_File --
   ---------------------------------

   overriding function Get_Tree_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False)
      return Language.Tree.Database.Tree_Language_Access
   is
      Index : Natural;
   begin
      Index := Get_Index_From_Language
        (Handler,
         Get_Language_From_File (Handler, Source_Filename, From_Project_Only));

      if Index /= 0 and then Handler.Languages (Index).Tree_Lang /= null then
         return Handler.Languages (Index).Tree_Lang;
      end if;

      return Language.Tree.Database.Unknown_Tree_Lang;
   end Get_Tree_Language_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Handler           : access Language_Handler_Record;
      Source_Filename   : GNATCOLL.VFS.Virtual_File;
      From_Project_Only : Boolean := False) return String
   is
      use GNATCOLL.Projects;
      Prop  : String_Property;
      Found : Boolean := False;
      Set   : File_Info_Set;
   begin
      if Source_Filename = No_File then
         --  This shouldn't happen, but some versions of GPS did save a
         --  language for the empty file in the properties database.
         --  Make sure we do not pick this up here.
         return "Unknown";
      end if;

      if not From_Project_Only then
         Get_Property (Prop, Source_Filename, "language", Found);
      end if;

      if Found then
         return Prop.Value.all;
      else
         --  With aggregate project, the same file could be found in several
         --  projects. However, it should always have the same language (unless
         --  one of the project uses another language, for instance before
         --  and after preprocessing). For now, we simply return the first
         --  language found.

         Set := Handler.Registry.Tree.Info_Set (Source_Filename);
         if Set.Is_Empty then
            return "";
         else
            declare
               F_Info : constant File_Info'Class :=
                 File_Info'Class (Set.First_Element);
            begin
               return F_Info.Language;
            end;
         end if;
      end if;
   end Get_Language_From_File;

   --------------------------
   -- Get_Language_By_Name --
   --------------------------

   function Get_Language_By_Name
     (Handler : access Language_Handler_Record;
      Name    : String) return Language.Language_Access
   is
      Index : constant Natural := Get_Index_From_Language (Handler, Name);
   begin
      if Index = 0 then
         return Unknown_Lang;
      else
         return Handler.Languages (Index).Lang;
      end if;
   end Get_Language_By_Name;

   -----------------------
   -- Register_Language --
   -----------------------

   procedure Register_Language
     (Handler   : access Language_Handler_Record;
      Lang      : access Language.Language_Root'Class;
      Tree_Lang : access Language.Tree.Database.Tree_Language'Class)
   is
      N     : constant String := To_Lower (Get_Name (Lang));
      Tmp   : Language_Info_Access;
      Index : Natural;
   begin
      if Handler.Languages /= null then
         Index := Get_Index_From_Language (Handler, N);
         if Index = 0 then
            Tmp := new Language_Info_Array
              (Handler.Languages'First .. Handler.Languages'Last + 1);
            Tmp (Handler.Languages'Range) := Handler.Languages.all;
            Unchecked_Free (Handler.Languages);
            Handler.Languages := Tmp;
            Index := Handler.Languages'Last;
         end if;
      else
         Handler.Languages := new Language_Info_Array (1 .. 1);
         Index := Handler.Languages'Last;
      end if;

      Set_Symbols (Lang, Handler.Symbols);

      Handler.Languages (Index) :=
        (Lang      => Language_Access (Lang),
         Tree_Lang => Tree_Language_Access (Tree_Lang));
   end Register_Language;

   ---------------------
   -- Known_Languages --
   ---------------------

   function Known_Languages
     (Handler : access Language_Handler_Record;
      Sorted  : Boolean := False) return GNAT.OS_Lib.Argument_List is
   begin
      if Handler.Languages /= null then
         declare
            procedure Move (From, To : Natural);
            function Lt (From, To : Natural) return Boolean;
            package Sort is new GNAT.Bubble_Sort_G (Move => Move, Lt => Lt);

            Result : Argument_List (1 .. Handler.Languages'Last);
            Tmp    : GNAT.Strings.String_Access;

            ----------
            -- Move --
            ----------

            procedure Move (From, To : Natural) is
            begin
               if From = 0 then
                  Result (To) := Tmp;
               elsif To = 0 then
                  Tmp := Result (From);
               else
                  Result (To) := Result (From);
               end if;
            end Move;

            --------
            -- Lt --
            --------

            function Lt (From, To : Natural) return Boolean is
            begin
               return Result (From).all < Result (To).all;
            end Lt;

         begin
            for Index in Result'Range loop
               Result (Index) := new String'
                 (Mixed_Case (Get_Name (Handler.Languages
                  (Index - 1 + Handler.Languages'First).Lang)));
            end loop;

            if Sorted then
               Sort.Sort (Result'Last);
            end if;

            return Result;
         end;

      else
         declare
            Result : Argument_List (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Known_Languages;

   ---------------------
   -- Languages_Count --
   ---------------------

   function Languages_Count
     (Handler : access Language_Handler_Record) return Natural is
   begin
      if Handler.Languages = null then
         return 0;
      else
         return Handler.Languages'Length;
      end if;
   end Languages_Count;

   ----------------------
   -- Get_Nth_Language --
   ----------------------

   function Get_Nth_Language
     (Handler : access Language_Handler_Record;
      Num     : Positive) return String is
   begin
      if Handler.Languages = null
        or else Num > Handler.Languages'Length
      then
         return "";
      else
         return Get_Name (Handler.Languages
           (Handler.Languages'First + Num - 1).Lang);
      end if;
   end Get_Nth_Language;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Handler : in out Language_Handler) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Language_Handler_Record'Class, Language_Handler);
   begin
      if Handler.Languages /= null then
         for L in Handler.Languages'Range loop
            Free (Handler.Languages (L).Lang);
         end loop;

         Unchecked_Free (Handler.Languages);
      end if;

      Unchecked_Free (Handler);
   end Destroy;

end Language_Handlers;
