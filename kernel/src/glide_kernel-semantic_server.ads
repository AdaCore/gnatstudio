--  This package provides routines to query information on source entities
--  such as cross references.

package Glide_Kernel.Semantic_Server is

   Max_Path_Len : constant Integer := 1024;
   --  Maximum length of a pathname.
   --  ??? Would be better to import max_path_len, but this would make
   --  all the types dynamic.

   subtype Pathname_Range is Natural range 0 .. Max_Path_Len;
   --  Type used to constrain the file string in Source_Location type below.

   type Source_Location
     (File_Length : Pathname_Range := Pathname_Range'First) is
   record
      File   : String (1 .. File_Length);
      Line   : Natural;
      Column : Natural;
   end record;
   --  Information returned by various earch routines in this package.
   --  A null string, or a Natural value of 0 means that the field has not
   --  been set.

   No_Location : constant Source_Location;
   --  No location constant used by the search routines in this package.

   type Location_Handler is access
     function
       (Kernel   : Kernel_Handle;
        Location : Source_Location) return Boolean;
   --  Handler used by search routines below.
   --  Kernel is a handle to the Kernel object.
   --  Location is the location of the current search result.
   --  This function should return True to continue the search, and False to
   --  stop it.

   type Complete_Handler is access
     function
       (Kernel : Kernel_Handle;
        Name   : String) return Boolean;
   --  Handler used by the complete routines below.
   --  Kernel is a handle to the Kernel object.
   --  Name is the current name found during the search.
   --  This function should return True to continue the search, and False to
   --  stop it.

   procedure Get_Declaration
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Location : out Source_Location);
   --  Given an entity (variable, procedure, ...) located at a specified
   --  file:line:col, return the location of its declaration.
   --  If Entity is a fully qualified entity, file:line:column can be
   --  omitted (using the default values).
   --  The result is stored in Location and is set to No_Location if the
   --  declaration or the entity could not be found.

   procedure Get_Body
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Location : out Source_Location);
   --  Given an entity (variable, procedure, ...) located at a specified
   --  file:line:col, return the location of its implementation.
   --  See Get_Declaration for more information on the parameters.

   procedure List_References
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Handler  : Location_Handler);
   --  Given an entity (variable, procedure, ...) located at a specified
   --  file:line:col, search for all references to this entity in the
   --  project associated with Kernel.
   --  See Get_Declaration for more information on the parameters.
   --  Handler is called for each match, and also called periodically
   --  with a No_Location parameter to allow for polling (e.g handling of
   --  Gtk events).

   procedure Next_Procedure
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural);
   --  Given the contents of a file/region, with the current Position, return
   --  the location of the next procedure, or 0 if there is no further
   --  procedure.

   procedure Prev_Procedure
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural);
   --  Given the contents of a file/region, with the current Position, return
   --  the location of the previous procedure, or 0 if there is no previous
   --  procedure.

   procedure Start_Of_Statement
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural);
   --  Given the contents of a file/region, with the current Position, return
   --  the location of the beginning of the current statement (e.g if
   --  statement, loop, ...).

   procedure End_Of_Statement
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural);
   --  Given the contents of a file/region, with the current Position, return
   --  the location of the end of the current statement (e.g if statement,
   --  loop, ...).

   function Get_Profile
     (Kernel     : Kernel_Handle;
      Subprogram : String;
      File       : String  := "";
      Line       : Natural := 0;
      Column     : Natural := 0) return String;
   --  Given subprogram located at a specified file:line:col, return its
   --  profile (parameters).
   --  If Subprogram is a fully qualified entity, file:line:column can be
   --  omitted (using the default values).

   procedure Complete_Name
     (Kernel   : Kernel_Handle;
      Name     : String;
      Contents : String;
      Handler  : Complete_Handler);
   --  Search all the names that begin with Name in the given buffer.
   --  Handler is called for each match, and also called periodically
   --  with a null string parameter to allow for polling (e.g handling of
   --  Gtk events).

   procedure Complete_Name
     (Kernel   : Kernel_Handle;
      Name     : String;
      Handler  : Complete_Handler);
   --  Search all the names that begin with Name in the project associated
   --  with Kernel.
   --  Handler is called for each match, and also called periodically
   --  with a null string parameter to allow for polling (e.g handling of
   --  Gtk events).

private

   No_Location : constant Source_Location :=
     (File_Length => 0, File => "", Line => 0, Column => 0);

end Glide_Kernel.Semantic_Server;
