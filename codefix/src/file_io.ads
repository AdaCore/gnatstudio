with Ada.Unchecked_Deallocation;

with Generic_List;

with Dynamic_Arrays;
with Text_Manager; use Text_Manager;

package File_Io is

   type File_Interface is new Text_Interface with private;
   --  This is the implementation of the text interface for using in simple
   --  text files.

   type Ptr_Text_File is access all File_Interface'Class;

   procedure Free (This : in out File_Interface);

   function Get
     (This   : File_Interface;
      Cursor : File_Cursor'Class;
      Len    : Natural)
      return String;

   function Get_Line
     (This   : File_Interface;
      Cursor : File_Cursor'Class)
      return String;

   procedure Replace
     (This      : in out File_Interface;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String);

   procedure Add_Line
     (This        : in out File_Interface;
      Cursor      : File_Cursor'Class;
      New_Line    : String);

   procedure Delete_Line
     (This : in out File_Interface;
      Cursor : File_Cursor'Class);

   procedure Save (This : File_Interface);

private

   package List_Str is new Generic_List (Dynamic_String);
   use List_Str;

   type File_Loaded is record
      Content : List_Str.List;
      Path    : Dynamic_String;
   end record;

   function Get_Line_Node
     (This : File_Loaded;
      Line : Positive) return List_Str.List_Node;

   type Ptr_File_Loaded is access File_Loaded;
   procedure Load
     (Container : File_Interface;
      New_File  : Ptr_File_Loaded;
      Path : String);
   --  Attention ! le second parametre a perdu sa pertienence !
   procedure Save (This : in out File_Loaded; Path : String := "");

   procedure Free (This : in out Ptr_File_Loaded);

   package File_Arrays is new Dynamic_Arrays (Ptr_File_Loaded, null);
   use File_Arrays;

   --  Penser a detruire le pointeur sur naturel
   type Ptr_Natural is access Natural;
   type Ptr_Files is access File_Arrays.Dynamic_Array;

   procedure Free is new
      Ada.Unchecked_Deallocation (Natural, Ptr_Natural);
   procedure Free is new
      Ada.Unchecked_Deallocation (File_Arrays.Dynamic_Array, Ptr_Files);

   type File_Interface is new Text_Interface with record
      Files        : Ptr_Files := new File_Arrays.Dynamic_Array;
      Number_Files : Ptr_Natural := new Natural'(0);
   end record;

   function Get_File_Loaded
     (This : File_Interface;
      File_Name : String)
     return Ptr_File_Loaded;

end File_Io;
