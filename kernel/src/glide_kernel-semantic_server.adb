package body Glide_Kernel.Semantic_Server is

   -------------------
   -- Complete_Name --
   -------------------

   procedure Complete_Name
     (Kernel   : Kernel_Handle;
      Name     : String;
      Contents : String;
      Handler  : Complete_Handler) is
   begin
      null;
   end Complete_Name;

   -------------------
   -- Complete_Name --
   -------------------

   procedure Complete_Name
     (Kernel   : Kernel_Handle;
      Name     : String;
      Handler  : Complete_Handler) is
   begin
      null;
   end Complete_Name;

   ----------------------
   -- End_Of_Statement --
   ----------------------

   procedure End_Of_Statement
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural) is
   begin
      Line   := 0;
      Column := 0;
   end End_Of_Statement;

   --------------
   -- Get_Body --
   --------------

   procedure Get_Body
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Location : out Source_Location) is
   begin
      Location := No_Location;
   end Get_Body;

   ---------------------
   -- Get_Declaration --
   ---------------------

   procedure Get_Declaration
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Location : out Source_Location) is
   begin
      Location := No_Location;
   end Get_Declaration;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Kernel     : Kernel_Handle;
      Subprogram : String;
      File       : String  := "";
      Line       : Natural := 0;
      Column     : Natural := 0) return String is
   begin
      return "";
   end Get_Profile;

   ---------------------
   -- List_References --
   ---------------------

   procedure List_References
     (Kernel   : Kernel_Handle;
      Entity   : String;
      File     : String  := "";
      Line     : Natural := 0;
      Column   : Natural := 0;
      Handler  : Location_Handler) is
   begin
      null;
   end List_References;

   --------------------
   -- Next_Procedure --
   --------------------

   procedure Next_Procedure
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural) is
   begin
      Line   := 0;
      Column := 0;
   end Next_Procedure;

   --------------------
   -- Prev_Procedure --
   --------------------

   procedure Prev_Procedure
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural) is
   begin
      Line   := 0;
      Column := 0;
   end Prev_Procedure;

   ------------------------
   -- Start_Of_Statement --
   ------------------------

   procedure Start_Of_Statement
     (Kernel   : Kernel_Handle;
      Contents : String;
      Position : Positive;
      Line     : out Natural;
      Column   : out Natural) is
   begin
      Line   := 0;
      Column := 0;
   end Start_Of_Statement;

end Glide_Kernel.Semantic_Server;

