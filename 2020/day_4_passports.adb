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
with Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Regpat; use GNAT.Regpat;
procedure Day_4_Passports
is
   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   Input_File  : File_Type;
   Output_File : File_Type;
   Output_File_Name : constant String := Ada.Command_Line.Argument (1) & ".out";

   Field : constant Pattern_Matcher := Compile ("([a-z][a-z][a-z]):([^ ]+)");

   Height : constant Pattern_Matcher := Compile ("([0-9]+)(cm|in)");

   Hair_Color : constant Pattern_Matcher := Compile ("#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]");

   Eye_Color : constant Pattern_Matcher := Compile ("amb|blu|brn|gry|grn|hzl|oth");

   ID : constant Pattern_Matcher := Compile ("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]");

   type Field_Labels is (Byr, Iyr, Eyr, Hgt, Hcl, Ecl, Pid, Other);
   subtype Required_Field_Labels is Field_Labels range Byr .. Pid;

   subtype String_3 is String (1 .. 3);
   Label_Image : constant array (Field_Labels) of String_3 :=
     ("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid");

   Alpha_Order_Map : constant array (Field_Labels) of Field_Labels :=
     (Byr, Other, Ecl, Eyr, Hcl, Hgt, Iyr, Pid);

   Passport_Count    : Integer := 0;
   Valid_Passports   : Integer := 0;
   Invalid_Passports : Integer := 0;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   if Ada.Directories.Exists (Output_File_Name) then
      Ada.Directories.Delete_File (Output_File_Name);
   end if;
   Create (Output_File, Out_File, Output_File_Name);

   Passports :
   loop
      declare
         Present      : array (Field_Labels) of Boolean := (others => False);
         Valid        : array (Field_Labels) of Boolean := (others => False);
         Found_Fields : array (Field_Labels) of Ada.Strings.Unbounded.Unbounded_String;
      begin
         Lines :
         loop
            exit Lines when End_Of_File (Input_File);

            declare
               Line  : constant String := Get_Line (Input_File);
               Last  : Integer         := Line'First - 1;

               Matches : Match_Array (0 .. 2);

               function Get (Loc : in Match_Location) return String
               is (Line (Loc.First .. Loc.Last));

               function To_Field return Field_Labels
               is
                  Label : constant String := Line (Matches (1).First .. Matches (1).Last);
               begin
                  if Label = "byr" then
                     return Byr;

                  elsif Label = "iyr" then
                     return Iyr;

                  elsif Label = "eyr" then
                     return Eyr;

                  elsif Label = "hgt" then
                     return Hgt;

                  elsif Label = "hcl" then
                     return Hcl;

                  elsif Label = "ecl" then
                     return Ecl;

                  elsif Label = "pid" then
                     return Pid;

                  else
                     return Other;
                  end if;
               end To_Field;

               function Valid_Field return Boolean
               is
                  Data : constant String := Line (Matches (2).First .. Matches (2).Last);
               begin
                  case To_Field is
                  when Byr =>
                     return Data'Length = 4 and then
                       (declare Year : constant Integer := Integer'Value (Data);
                        begin 1920 <= Year and Year <= 2002);

                  when Iyr =>
                     return Data'Length = 4 and then
                       (declare Year : constant Integer := Integer'Value (Data);
                        begin 2010 <= Year and Year <= 2020);

                  when Eyr =>
                     return Data'Length = 4 and then
                       (declare Year : constant Integer := Integer'Value (Data);
                        begin 2020 <= Year and Year <= 2030);

                  when Hgt =>
                     declare
                        Matches : Match_Array (0 .. 2);
                     begin
                        Match (Height, Data, Matches);
                        return (Matches (1) /= No_Match and Matches (2) /= No_Match) and then
                          (declare Value : constant Integer := Integer'Value
                             (Data (Matches (1).First .. Matches (1).Last));
                           begin
                              (if Data (Matches (2).First .. Matches (2).Last) = "cm"
                               then 150 <= Value and Value <= 193
                               else 59 <= Value and Value <= 76));
                     end;

                  when Hcl =>
                     return Match (Hair_Color, Data);

                  when Ecl =>
                     return Match (Eye_Color, Data);

                  when Pid =>
                     return Match (ID, Data);

                  when Other =>
                     return True;
                  end case;
               end Valid_Field;

            begin
               exit Lines when Line = "";

               Fields :
               loop
                  Match (Field, Line, Matches, Data_First => Last + 1, Data_Last => Line'Last);
                  exit Fields when Matches (0) = No_Match; -- end of line

                  declare
                     Label : constant Field_Labels := To_Field;
                  begin
                     Found_Fields (Label)  := To_Unbounded_String (Get (Matches (2)));
                     Present (Label) := True;
                     Valid (Label) := Valid_Field;

                     if Verbose then
                        Put_Line ("field: '" & Line (Matches (0).First .. Matches (0).Last) & "'" &
                                    (if Valid (Label) then "" else " invalid"));
                     end if;
                  end;
                  Last := Matches (0).Last;
               end loop Fields;
            end;
         end loop Lines;
         Passport_Count := @ + 1;
         for Label in Field_Labels loop
            declare
               Mapped_Label : constant Field_Labels := Alpha_Order_Map (Label);
            begin
               if Present (Mapped_Label) then
                  Put_Line (Output_File, Label_Image (Mapped_Label) & ":" & To_String (Found_Fields (Mapped_Label)));
               end if;
            end;
         end loop;
         New_Line (Output_File);

         if Valid (Byr .. Pid) = (Required_Field_Labels => True) then
            if Verbose then
               Put_Line ("valid passport");
            end if;
            Valid_Passports := @ + 1;
         else
            if Verbose then
               Put_Line ("invalid passport line" & Positive_Count'Image (Line (Input_File) - 1));
            end if;
            Invalid_Passports := @ + 1;
         end if;
      end;
      exit Passports when End_Of_File (Input_File);
   end loop Passports;

   Put_Line ("passports:" & Passport_Count'Image);
   Put_Line ("valid    :" & Valid_Passports'Image);
   Put_Line ("invalid  :" & Invalid_Passports'Image);
end Day_4_Passports;
