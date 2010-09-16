package com.adacore.example;

import com.adacore.example.Hello.*;
import com.adacore.example.Hello.Printer;
import com.adacore.example.Hello.Ada_Printer;
import com.adacore.example.Hello.Hello_Package;
import com.adacore.example.Standard.AdaString;

public class @_Main_Name_@ {

   public static class Java_Printer extends Printer {

      public void Print_On_Console (AdaString V) {
         System.out.println ("[Java] " + V.toString ());
      }

   }

   public static void main (String [] args) {
      Hello_Package.Print_Messages (new Ada_Printer ());
      Hello_Package.Print_Messages (new Java_Printer ());
   }

};

