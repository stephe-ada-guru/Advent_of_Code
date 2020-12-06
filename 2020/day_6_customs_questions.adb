--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO; use Ada.Text_IO;
procedure Day_6_Customs_Questions
is
   use Ada.Containers;

   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   Input_File  : File_Type;

   package Char_Sets is new Ada.Containers.Ordered_Sets (Character);

   function Image (Item : in Char_Sets.Set) return String
   is
      Result : String (1 .. Integer (Item.Length) + 2);
      Next   : Integer := Result'First;
   begin
      Result (Next) := ''';
      Next := @ + 1;
      for C of Item loop
         Result (Next) := C;
         Next := @ + 1;
      end loop;
      Result (Next) := ''';
      return Result;
   end Image;

   Total_Yes : Count_Type := 0;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   Groups :
   loop
      declare
         Group_Yes : Char_Sets.Set;
         First_Person : Boolean := True;
      begin
         Persons :
         loop
            exit Persons when End_Of_File (Input_File);

            declare
               Person  : constant String := Get_Line (Input_File);
               Person_Yes : Char_Sets.Set;
            begin
               exit Persons when Person = "";

               for C of Person loop
                  Person_Yes.Include (C);
               end loop;
               if First_Person then
                  Group_Yes    := Person_Yes;
                  First_Person := False;
               else
                  Group_Yes.Intersection (Person_Yes);
               end if;
               if Verbose then
                  Put_Line ("person_yes " & Image (Person_Yes));
                  Put_Line ("group_yes  " & Image (Group_Yes));
               end if;
            end;
         end loop Persons;

         Total_Yes := @ + Group_Yes.Length;
      end;
      exit Groups when End_Of_File (Input_File);
   end loop Groups;
   Put_Line ("yes:" & Total_Yes'Image);
end Day_6_Customs_Questions;
