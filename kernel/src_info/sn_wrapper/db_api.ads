package DB_API is

   type DB_File is limited private;
   --  Database file descriptor.

   type Cursor_Position is (First, Last, By_Key);
   --  Cursor positions for Set_Cursor.

   type Cursor_Movement is (Prev, Next, Next_By_Key);
   --  Cursor movements fro Get_Pair

   Field_Sep : constant Character := Character'Val (1);
   --  Standard field separator

   type CSF is limited private;
   --  CSV stands for "Character Separated Fields".
   --  Represents key or data value ("CSF" in term of SN team).
   --  Value can contain 0 or more string fileds.
   --  Each field can be retrieved by Get_Field function.
   --  ("data base thang" in terms of SN team).

   type Pair is record
      Key   : CSF;
      Data  : CSF;
   end record;
   --  Type for key/data pair retrieved from database by Get_Pair
   --  operation.
   type Pair_Ptr is access all Pair;

   procedure Open  (DB : out DB_File; File_Name : String);
   --  Opens specified file as database file. Upon successful
   --  completion DB is initialized. Otherwise DB_Open_Error
   --  exception is thrown.
   --  Sets cursor to the first key/data pair in database (see
   --  Set_Cursor).

   function Is_Open (DB : DB_File) return Boolean;
   --  Returns True if given DB ws successfully open,
   --  False otherwise.

   procedure Close (DB : in out DB_File);
   --  Closes specified DB file and frees underlying resources.
   --  Ignores uninitialized DB.
   --  Throws DB_Close_Error if failed.

   procedure Set_Cursor
     (DB          : DB_File;
      Position    : Cursor_Position;
      Key         : String  := "";
      Exact_Match : Boolean := True);
   --  Sets cursor position in given database.
   --  If Position = First, then cursor is set to the first key/data
   --  pair in database, Key is ignored.
   --  If Position = Last, then cursor is set to the last key/data
   --  pair in database, Key is ignored.
   --  If Position = By_Key, then cursor is set to the first key/data
   --  pair, where key is:
   --    The first key equals to specified Key, if Exact_Match is True.
   --    The smallest one greater than or equals to specified Key, if
   --    Exact_Match is False.
   --  Throws DB_Error if DB was not opened.

   function Get_Pair
     (DB       : DB_File;
      Movement : Cursor_Movement := Next) return Pair_Ptr;
   --  Gets key/data pair from database from cursor position and
   --  changes cursor position to:
   --    Next key/data pair, if Movement is Next.
   --    Previous key/data pair, if Movement is Prev.
   --    Next key/data pair, where key is satisfying conditions set by
   --    Set_Cursor operations, if Movement is Next_By_Key.
   --  Returns null if there are no key/data pairs more.
   --  If Movement is Next_By_Key and cursor was not set by Set_Cursor
   --  with Position = By_Key then Get_Pair returns null;
   --  Throws DB_Error if DB was not opened or error occurred.

   procedure Free (The_Pair : in out Pair_Ptr);
   --  Releases specified pair and key/data values.

   function Get_Field_Count (The_CSF : CSF) return Natural;
   --  Returns number of fields in specified CSF.

   function Get_Field (The_CSF : CSF; Index : Positive) return String;
   --  Returns field from CSF with specified index.
   --  Throws Index_Out_Of_Range if incorrect index specified (
   --  i.e. Index > Get_Field_Count (CSF);

   function Get_Field_Length (The_CSF : CSF; Index : Positive)
         return Integer;
   --  Returns length of field from CSF with specified index.


   function Get_All_Fields
     (The_CSF : CSF;
      Separator : Character := ' ') return String;
   --  Returns all fields, separated by specified separator;

   function Get_Total_Length (The_CSF : CSF) return Natural;
   --  Return summary length of all fields

   DB_Error             : exception;
   DB_Open_Error        : exception;
   DB_Close_Error       : exception;
   Index_Out_Of_Range   : exception;

private

   type DB_File_Record is null record;
   type DB_File is access DB_File_Record;
   pragma Convention (C, DB_File);

   function Is_Null (DB : DB_File) return Boolean;
   --  checks if DB is NULL (in terms of C)

   function Error_Message (DB : DB_File) return String;
   --  return string decribing the last error for given DB

   type CSF_Record is null record;
   type CSF is access CSF_Record;
   pragma Convention (C, CSF);

end DB_API;
