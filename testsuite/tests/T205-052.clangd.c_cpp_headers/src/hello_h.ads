pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package hello_h is

   package Class_Hello is
      type Hello is tagged limited record
         MyInt : aliased int;  -- hello.h:9
         MyInt2 : aliased int;  -- hello.h:10
      end record;
      pragma Import (CPP, Hello);

      function New_Hello return Hello;  -- hello.h:4
      pragma CPP_Constructor (New_Hello, "_ZN5HelloC1Ev");

      procedure Delete_Hello (this : access Hello);  -- hello.h:5
      pragma Import (CPP, Delete_Hello, "_ZN5HelloD1Ev");

      procedure DoSomething
        (this : access Hello'Class;
         one : int;
         two : int);  -- hello.h:7
      pragma Import (CPP, DoSomething, "_ZN5Hello11DoSomethingEii");
   end;
   use Class_Hello;
end hello_h;
