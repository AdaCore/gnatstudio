with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Prj;                  use Prj;
with Prj.Tree;             use Prj.Tree;
with Src_Info;             use Src_Info;
with Src_Info.Queries;     use Src_Info.Queries;

with Language_Handlers;       use Language_Handlers;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Test_Utils;              use Test_Utils;
with Html_Output;             use Html_Output;


package body Work_on_Source is

   Max_Line_Length : Natural := 80;

   package GOL renames GNAT.OS_Lib;
   package ASU renames Ada.Strings.Unbounded;


   --------------------
   -- Process_Source --
   --------------------

   procedure Process_Source
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Package_Name    : GNAT.OS_Lib.String_Access;
      Exception_List  : Type_Exception_List.List;
      Subprogram_List : Type_Subprogram_List.List;
      Type_List       : Type_Type_List.List) is

   begin
      Header(Doc_File, Package_Name.all);
      Process_Exceptions  (Doc_File, Doc_File_Type, Exception_List);
      Process_Types       (Doc_File, Doc_File_Type, Type_List);
      Process_Subprograms (Doc_File, Doc_File_Type, Subprogram_List);
      Footer (Doc_File);
   end;


   ------------------------
   -- Process_Exceptions --
   ------------------------

   procedure Process_Exceptions
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Exception_List : Type_Exception_List.List) is

      Exception_Node                  : Type_Exception_List.List_Node;
      Name, Short_Name, Rename, Descr : Unbounded_String;

      package TEL renames Type_Exception_List;

      procedure Document
        (Name        : Unbounded_String;
         Rename      : Unbounded_String;
         Description : Unbounded_String) is
      begin
         Doc_HTML_Exception (Doc_File,
                             To_String(Name),
                             To_String(Rename),
                             To_String(Description));
      end;

   begin
      if not TEL.Is_Empty (Exception_List) then

         Doc_HTML_Subtitle (Doc_File, "Exceptions: ");

         Exception_Node := TEL.First (Exception_List);
         for J in 1..TEL.Length (Exception_List) loop
            Name := To_Unbounded_String(TEL.Data(Exception_Node).Name.all);
            if Index (Name, ".") > 0 then --not a renamed exception! (sure enought?)
               Short_Name := To_Unbounded_String(TEL.Data(Exception_Node).Short_Name.all);
               Rename := Exception_renames (TEL.Data(Exception_Node).File_Name.all,
                                           TEL.Data(Exception_Node).Line);
               Descr := Extract_Comment (TEL.Data(Exception_Node).File_Name.all,
                                      TEL.Data(Exception_Node).Line);
               Document(Short_Name, Rename, Descr);
            end if;
            Exception_Node := TEL.Next(Exception_Node);
         end loop;
      end if;
   end;


   -------------------
   -- Process_Types --
   -------------------

   procedure Process_Types
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Type_List : Type_Type_List.List) is

      Type_Node                          : Type_Type_List.List_Node;
      Name, Short_Name, Type_Name, Descr : Unbounded_String;

      package TTL renames Type_Type_List;

      procedure Document
        (Name        : Unbounded_String;
         Description : Unbounded_String;
         Type_Name   : Unbounded_String) is
      begin
         Doc_HTML_Type (Doc_File,
                        To_String(Name),
                        To_String(Type_Name),
                        To_String(Description));
      end;

   begin
      if not TTL.Is_Empty (Type_List) then

         Doc_HTML_Subtitle (Doc_File, "Types: ");

         Type_Node := TTL.First (Type_List);
         for J in 1..TTL.Length (Type_List) loop
            Name := To_Unbounded_String(TTL.Data(Type_Node).Name.all);
            if Index (Name, ".") > 0 then --not a renamed exception! (sure enought?)
               Short_Name := To_Unbounded_String(TTL.Data(Type_Node).Short_Name.all);
               Descr := Extract_Comment (TTL.Data(Type_Node).File_Name.all,
                                         TTL.Data(Type_Node).Line);
               Type_Name := To_Unbounded_String(TTL.Data(Type_Node).Type_Name.all);
               Document(Short_Name, Descr, Type_Name);
            end if;
            Type_Node := TTL.Next(Type_Node);
         end loop;
      end if;
   end;


   -------------------------
   -- Process_Subprograms --
   -------------------------

   procedure Process_Subprograms
     (Doc_File        : File_Type;
      Doc_File_Type   : String;
      Subprogram_List : Type_Subprogram_List.List) is

      Subprogram_Node                    : Type_Subprogram_List.List_Node;
      Name, Short_Name, Type_Name, Descr : Unbounded_String;

      package TSL renames Type_Subprogram_List;

      procedure Document
        (Name        : Unbounded_String;
         Type_Name   : Unbounded_String;
         Description : Unbounded_String) is
      begin
         Doc_HTML_Subprogram (Doc_File,
                              To_String(Name),
                              To_String(Type_Name),
                              To_String(Description));

      end;

   begin
      if not TSL.Is_Empty (Subprogram_List) then

         Doc_HTML_Subtitle (Doc_File, "Subprograms: ");

         Subprogram_Node := TSL.First (Subprogram_List);
         for J in 1..Type_Subprogram_List.Length (Subprogram_List) loop
            Name := To_Unbounded_String(TSL.Data(Subprogram_Node).Name.all);
            if Index (Name, ".") > 0
            --not a renamed exception: make document! (sure enought?)
            then
               Short_Name := To_Unbounded_String(TSL.Data(Subprogram_Node).Short_Name.all);
               Type_Name := To_Unbounded_String(TSL.Data(Subprogram_Node).Type_Name.all);
               Descr := Extract_Comment (TSL.Data(Subprogram_Node).File_Name.all,
                                      TSL.Data(Subprogram_Node).Line);
               Document(Short_Name, Type_Name, Descr);
            end if;
            Subprogram_Node := TSL.Next(Subprogram_Node);
         end loop;
      end if;
   end;


   ---------------------------
   -- Get_Next_Comment_Line --
   ---------------------------

   function Get_Next_Comment_Line
     (File : File_Type) return Unbounded_String is

      Last          : Natural;
      New_Line      : String (1..Max_Line_Length);
      New_Unbounded : Unbounded_String;


      function Line_Is_Comment
        (Line : String;
         Last : Natural) return boolean is
      begin
         if Line'Length >5 then
            for J in 1..Last-3 loop
               if Line(J) = '-' and Line(J+1) = '-' then return true;
               elsif Line(J) /= ' ' then return false;
               end if;
            end loop;
         end if;
         return false;
      end;

      function Kill_Prefix
        (Line : String) return Unbounded_String is
         J : Natural;
      begin
         J := 1;
         while (Line(J) /= '-' and Line(J+1) /= '-') loop
            J := J + 1;
         end loop;
         return Delete (To_Unbounded_String(Line & ' '), 1, J+2);
      end;


   begin
      Ada.Text_IO.Get_Line (File, New_Line, Last);

      if Line_Is_Comment (New_Line, Last) then
         return Kill_Prefix(New_Line (1..Last));
      else return To_Unbounded_String("");
      end if;

   end;


   ----------------
   -- Go_To_Line --
   ----------------

   procedure Go_To_Line
     (File : File_Type;
      Line : Natural) is
      Dummy : String (1..Max_Line_Length);
      Last  : Natural;
   begin
      for J in 1..Line loop Get_Line (File, Dummy, Last); end loop;
      --how convert Natural->Count in order to use Skip?
   end;


   ---------------------
   -- Extract_Comment --
   ---------------------

   function Extract_Comment
     (File_Name : String;
      Line      : Natural)
     return Unbounded_String is

      File        : File_Type;
      Text        : Unbounded_String;
      Result_Text : Unbounded_String;

   begin
      Open (File, In_File, File_Name);

      --make one unbounded string of all comment lines of this entity
      Text := To_Unbounded_String("nothing");
      Go_To_Line (File, Line);
      while Text /= "" loop
         text := Get_Next_Comment_Line (File);
         Result_Text := Result_Text & Text;
      end loop;

      Close (File);
      return Result_Text;
   end;


   -----------------------
   -- Exception_Renames --
   -----------------------

   function Exception_Renames
     (File_Name : String;
      Line      : Natural)
     return Unbounded_String is

      File        : File_Type;
      Last        : Natural;
      Text        : Unbounded_String;
      Result_Text : Unbounded_String;
      New_Line    : String (1..Max_Line_Length);

   begin
      Open (File, In_File, File_Name);

      Go_To_Line (File, Line-1);
      Ada.Text_IO.Get_Line (File, New_Line, Last);
      Text := To_Unbounded_String(New_Line);

      if Index(Text,"renames") > 0 then
         Result_Text := To_Unbounded_String( " " & New_Line(Index(text,"renames")..Last-1));

         --delete ";" at the end of the line
         --Delete (Result_Text, Index(Result_Text, ";") , 1);
      else
         Result_Text := To_Unbounded_String ("");
      end if;

      Close (File);
      return Result_Text;
   end;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Exception_Information) is
   begin
         Put_Line ("Free!");
   end Free;

   procedure Free (X : in out Type_Information) is
   begin
         Put_Line ("Free! ");
   end Free;

   procedure Free (X : in out Subprogram_Information) is
   begin
         Put_Line ("Free! ");
   end Free;


end Work_on_Source;
