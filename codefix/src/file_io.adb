with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body File_Io is

   ----------
   -- Free --
   ----------

   procedure Free (This : in out File_Interface) is
   begin
      Free (This.Files.all);
      Free (This.Files);
      Free (This.Number_Files);
   end Free;

   ---------
   -- Get --
   ---------

   function Get
     (This   : File_Interface;
      Cursor : File_Cursor'Class;
      Len    : Natural)
     return String is

      File : Ptr_File_Loaded;

   begin
      File := Get_File_Loaded (This, Cursor.File_Name.all);

      if File = null then
         File := new File_Loaded;
         Load (This, File, Cursor.File_Name.all);
      end if;

      return Data (Get_Line_Node (File.all, Cursor.Line)).all
        (Cursor.Col .. Cursor.Col + Len - 1);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This   : File_Interface;
      Cursor : File_Cursor'Class)
      return String is

      Element : Dynamic_String;
      File    : Ptr_File_Loaded;

   begin
      File := Get_File_Loaded (This, Cursor.File_Name.all);

      if File = null then
         File := new File_Loaded;
         Load (This, File, Cursor.File_Name.all);
      end if;

      Element := Data (Get_Line_Node (File.all, Cursor.Line));
      return Element.all (Cursor.Col .. Element.all'Length);
   end Get_Line;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out File_Interface;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String) is

      Element  : Dynamic_String;
      File     : Ptr_File_Loaded;



   begin

      File := Get_File_Loaded (This, Cursor.File_Name.all);

      if File = null then
         File := new File_Loaded;
         Load (This, File, Cursor.File_Name.all);
      end if;
      Element := Data (Get_Line_Node (File.all, Cursor.Line));

      Set_Data
       (Get_Line_Node (File.all, Cursor.Line),
        new String '(Element.all (1 .. Cursor.Col - 1) &
                     New_Value &
                     Element.all (Cursor.Col + Len .. Element.all'Length)));

   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out File_Interface;
      Cursor      : File_Cursor'Class;
      New_Line    : String) is

      File : Ptr_File_Loaded;

   begin
      File := Get_File_Loaded (This, Cursor.File_Name.all);

      if File = null then
         File := new File_Loaded;
         Load (This, File, Cursor.File_Name.all);
      end if;

      if Cursor.Line = 0 then
         Prepend (File.Content, new String'(New_Line));
      else
         Append
           (File.Content,
            Get_Line_Node (File.all, Cursor.Line),
            new String'(New_Line));
      end if;

   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out File_Interface;
      Cursor : File_Cursor'Class) is

      File        : Ptr_File_Loaded;
      Delete_Node : List_Node;

   begin
      File := Get_File_Loaded (This, Cursor.File_Name.all);

      if File = null then
         File := new File_Loaded;
         Load (This, File, Cursor.File_Name.all);
      end if;

      Delete_Node := Get_Line_Node (File.all, Cursor.Line);
      Remove_Nodes
        (File.Content,
         Prev (File.Content, Delete_Node),
         Delete_Node);

   end Delete_Line;

   -------------------
   -- Get_Line_Node --
   -------------------

   function Get_Line_Node
     (This : File_Loaded;
      Line : Positive) return List_Str.List_Node is

      Current_Node : List_Str.List_Node;

   begin
      Current_Node := First (This.Content);
      for J in 1 .. Line - 1 loop
         Current_Node := Next (Current_Node);
      end loop;

      return Current_Node;
   end Get_Line_Node;


   ----------
   -- Load --
   ----------

   procedure Load
     (Container : File_Interface;
      New_File  : Ptr_File_Loaded;
      Path      : String) is

      File     : File_Type;
      Line_Red : Dynamic_String;

   begin
      Affect (New_File.Path, Path);
      Free (New_File.Content);
      Open (File, In_File, Path);
      while not End_Of_File (File) loop
         Line_Red := null; --  to not to erase the line red before
         Get_Line (File, Line_Red);
         Append (New_File.Content, Line_Red);
      end loop;
      Container.Number_Files.all := Container.Number_Files.all + 1;
      Set_Element (Container.Files.all, New_File, Container.Number_Files.all);
      Close (File);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (This : File_Interface) is
   begin
      for J in 1 .. This.Number_Files.all loop
         Save (Get_Element (This.Files.all, J).all);
      end loop;
   end Save;

   ----------
   -- Save --
   ----------

   procedure Save (This : in out File_Loaded; Path : String := "") is
      File         : File_Type;
      Current_Node : List_Str.List_Node;

   begin
      if Path = "" then
         Open (File, Out_File, This.Path.all);
      else
         Create (File, Out_File, Path);
      end if;


      Current_Node := First (This.Content);
      while Current_Node /= Null_Node loop
         Put_Line (File, Data (Current_Node));
         Current_Node := Next (Current_Node);
      end loop;

      Close (File);
   end Save;

   ---------------------
   -- Get_File_Loaded --
   ---------------------

   function Get_File_Loaded
     (This : File_Interface;
      File_Name : String)
     return Ptr_File_Loaded is
   begin
      for J in 1 .. This.Number_Files.all loop
         if Get_Element (This.Files.all, J).Path.all = File_Name then
            return Get_Element (This.Files.all, J);
         end if;
      end loop;
      return null;
   end Get_File_Loaded;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_File_Loaded) is
      procedure Delete is new Ada.Unchecked_Deallocation
        (File_Loaded,
         Ptr_File_Loaded);
   begin
      if This /= null then
         Free (This.Content);
         Free (This.Path);
         Delete (This);
      end if;
   end Free;

end File_Io;
