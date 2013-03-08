-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Logger is

   protected File is
      procedure Close;
      --  Close the log file.

      procedure Open
        (Name : in String);
      --  Open the Name log file.

      procedure Put
        (Message : in String;
         Prefix  : in String);
      --  Write Prefix Message to log file.
   private
      Log : Ada.Text_IO.File_Type;
   end File;

   protected body File is

      -------------
      --  Close  --
      -------------

      procedure Close
      is
      begin
         Ada.Text_IO.Close (File => Log);
      end Close;

      ------------
      --  Open  --
      ------------

      procedure Open
        (Name : in String)
      is
      begin
         Ada.Text_IO.Open (File => Log,
                           Name => Name,
                           Mode => Ada.Text_IO.Append_File);
      exception
         when others =>
            Ada.Text_IO.Create (File => Log,
                                Name => Name,
                                Mode => Ada.Text_IO.Out_File);
      end Open;

      -----------
      --  Put  --
      -----------

      procedure Put
        (Message : in String;
         Prefix  : in String)
      is
         Position : Positive := Message'First;
      begin
         Ada.Text_IO.Put (Log, " " & Prefix & " : ");
         loop
            loop
               exit when Position > Message'Last;
               exit when Message (Position) = Ada.Characters.Latin_1.LF;
               exit when Message (Position) = Ada.Characters.Latin_1.CR;

               Ada.Text_IO.Put (Log, Message (Position));
               Position := Position + 1;
            end loop;

            loop
               exit when Position > Message'Last;
               exit when Message (Position) /= Ada.Characters.Latin_1.LF and
                         Message (Position) /= Ada.Characters.Latin_1.CR;
               Position := Position + 1;
            end loop;

            Ada.Text_IO.New_Line (Log);
            exit when Position > Message'Last;
            Ada.Text_IO.Put (Log, "_" & Prefix & "_: ");
         end loop;
      end Put;
   end File;

   -------------
   --  Close  --
   -------------

   procedure Close is
   begin
      File.Close;
   end Close;

   -------------
   --  Debug  --
   -------------

   procedure Debug
     (Message : in String)
   is
   begin
      File.Put ("Debug", Message);
   end Debug;

   -------------
   --  Error  --
   -------------

   procedure Error
     (Message : in String)
   is
   begin
      File.Put ("Error", Message);
   end Error;

   ------------
   --  Info  --
   ------------

   procedure Info
     (Message : in String)
   is
   begin
      File.Put ("Info", Message);
   end Info;

   ------------
   --  Open  --
   ------------

   procedure Open
     (File_Name : in String)
   is
   begin
      File.Open (File_Name);
   end Open;

   ---------------
   --  Warning  --
   ---------------

   procedure Warning
     (Message : in String)
   is
   begin
      File.Put ("Warning", Message);
   end Warning;

end Logger;
