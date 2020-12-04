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
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;
procedure Day_2_Password_Policy
is
   Numeric   : constant Ada.Strings.Maps.Character_Set := Ada.Strings.Maps.To_Set ("0123456789");

   Input_File              : File_Type;
   Sled_Rental_Count_Valid : Integer := 0;
   Toboggan_Count_Valid    : Integer := 0;

   function Sled_Rental_Valid
     (Password  : in String;
      Low, High : in Integer;
      Letter    : in Character)
     return Boolean
   is
      Letter_Count : Integer := 0;
   begin
      for C of Password loop
         if C = Letter then
            Letter_Count := @ + 1;
         end if;
      end loop;

      return Letter_Count in Low .. High;
   end Sled_Rental_Valid;

   function Toboggan_Valid
     (Password  : in String;
      Low, High : in Integer;
      Letter    : in Character)
     return Boolean
   is
      Match_Count : Integer := 0;
   begin
      if Password (Low) = Letter then
         Match_Count := @ + 1;
      end if;

      if Password (High) = Letter then
         Match_Count := @ + 1;
      end if;

      return Match_Count = 1;
   exception
   when Constraint_Error =>
      raise Constraint_Error with Low'Image & " or" & High'Image & " not in password '" & Password & "' range" &
        Password'First'Image & " .." & Password'Last'Image;
   end Toboggan_Valid;

begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   loop
      exit when End_Of_File (Input_File);

      declare
         Line : constant String  := Get_Line (Input_File);
         Last : Integer := Line'First - 1;

         function Get_Int (Skip : in Integer) return Integer
         is
            use Ada.Strings.Fixed;
            use Ada.Strings;
            First : constant Integer := Last + 1 + Skip;
         begin
            Last := -1 + Index (Source => Line (First .. Line'Last), Set => Numeric, Test => Outside);
            return Integer'Value (Line (First .. Last));
         end Get_Int;

         function Slide (Item : in String) return String
         with Post => Slide'Result'First = 1
         is begin
            return Result : String (1 .. Item'Length) do
               for I in Item'Range loop
                  Result (I - Item'First + 1) := Item (I);
               end loop;
            end return;
         end Slide;

         Low      : constant Integer   := Get_Int (Skip => 0);
         High     : constant Integer   := Get_Int (Skip => 1);
         Letter   : constant Character := Line (Last + 2);
         Password : constant String    := Slide (Line (Last + 5 .. Line'Last));
      begin
         if Sled_Rental_Valid (Password, Low, High, Letter) then
            Put_Line ("sled_rental valid: " & Line);
            Sled_Rental_Count_Valid := @ + 1;
         end if;
         if Toboggan_Valid (Password, Low, High, Letter) then
            Put_Line ("toboggan valid: " & Line);
            Toboggan_Count_Valid := @ + 1;
         end if;
      exception
      when E : Constraint_Error =>
         Put_Line (Line & ": " & Ada.Exceptions.Exception_Message (E));
      end;
   end loop;

   Put_Line ("sled_rental valid:" & Sled_Rental_Count_Valid'Image);
   Put_Line ("toboggan valid:" & Toboggan_Count_Valid'Image);
end Day_2_Password_Policy;
