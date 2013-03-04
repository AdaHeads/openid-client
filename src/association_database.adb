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

with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with AWS.OpenID.Log;

package body Association_Database is

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => AWS.OpenID.Security.Association_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      Element_Type    => AWS.OpenID.Security.Association,
      "="             => AWS.OpenID.Security."=");

   protected Database is
      procedure Clean_Up;

      function Has
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return Boolean;

      procedure Insert
        (Item : in AWS.OpenID.Security.Association);

      procedure Load
        (File_Name : in String);

      function Look_Up
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return AWS.OpenID.Security.Association;

      procedure Save
        (File_Name : in String);
   private
      Associations : Maps.Map := Maps.Empty_Map;
   end Database;

   ----------------
   --  Database  --
   ----------------

   protected body Database is

      ----------------
      --  Clean_Up  --
      ----------------

      procedure Clean_Up
      is
      begin
         raise Program_Error;
      end Clean_Up;

      -----------
      --  Has  --
      -----------

      function Has
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return Boolean
      is
      begin
         AWS.OpenID.Log.Info
           (Message => "Looking up if <" &
              Ada.Strings.Unbounded.To_String (Handle) &
              "> exists in the association database.");

         return Maps.Contains (Container => Associations,
                               Key       => Handle);
      end Has;

      --------------
      --  Insert  --
      --------------

      procedure Insert
        (Item : in AWS.OpenID.Security.Association)
      is
         Key : constant AWS.OpenID.Security.Association_Handle
           := AWS.OpenID.Security.Handle (Item);
      begin
         Maps.Insert (Container => Associations,
                      Key       => Key,
                      New_Item  => Item);
      exception
         when others =>
            AWS.OpenID.Log.Error
              (Message => "Failed to insert <" &
                          AWS.OpenID.Security.To_String (Item) &
                          "> into the association database with the handle <" &
                          Ada.Strings.Unbounded.To_String (Key) & ">.");
            AWS.OpenID.Log.Error
              (Message => "Current database capacity: " &
                          Maps.Capacity (Associations)'Img);
            AWS.OpenID.Log.Error
              (Message => "Hash value: " &
                          Ada.Strings.Unbounded.Hash (Key)'Img);
            raise;
      end Insert;

      ------------
      --  Load  --
      ------------

      procedure Load (File_Name : in String)
      is
         use Ada.Streams.Stream_IO;

         File    : Ada.Streams.Stream_IO.File_Type;
         Source  : Ada.Streams.Stream_IO.Stream_Access;
         Key     : AWS.OpenID.Security.Association_Handle;
         Element : AWS.OpenID.Security.Association;
      begin
         Open (File => File,
               Name => File_Name,
               Mode => In_File);
         Source := Stream (File);

         while not End_Of_File (File) loop
            Key     := AWS.OpenID.Security.Association_Handle'Input (Source);
            Element := AWS.OpenID.Security.Association'Input (Source);
            Maps.Insert (Container => Associations,
                         Key       => Key,
                         New_Item  => Element);
         end loop;

         Close (File => File);
      end Load;

      ---------------
      --  Look_Up  --
      ---------------

      function Look_Up
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return AWS.OpenID.Security.Association
      is
      begin
         AWS.OpenID.Log.Info
           (Message => "Looking <" & Ada.Strings.Unbounded.To_String (Handle) &
                       "> up in the association database.");
         return Maps.Element (Container => Associations,
                              Key       => Handle);
      end Look_Up;

      procedure Save (File_Name : in     String) is
         use Ada.Streams.Stream_IO;

         procedure Save
           (Position : in Maps.Cursor);
         --  TODO: write comment

         File    : Ada.Streams.Stream_IO.File_Type;
         Target  : Ada.Streams.Stream_IO.Stream_Access;

         ------------
         --  Save  --
         ------------

         procedure Save
           (Position : in Maps.Cursor)
         is
         begin
            AWS.OpenID.Security.Association_Handle'Output
              (Target,
               Maps.Key (Position));
            AWS.OpenID.Security.Association'Output (Target,
                                                    Maps.Element (Position));
         end Save;
      begin
         Create (File => File,
                 Name => File_Name);
         Target := Stream (File);
         Maps.Iterate (Container => Associations,
                       Process   => Save'Access);
         Close (File => File);
      end Save;

   end Database;

   ----------------
   --  Clean_Up  --
   ----------------

   procedure Clean_Up
   is
   begin
      Database.Clean_Up;
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Association_Database.Clean_Up: " &
              Ada.Exceptions.Exception_Name (E) & " (" &
              Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Clean_Up;

   -----------
   --  Has  --
   -----------

   function Has
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return Boolean
   is
   begin
      return Database.Has (Handle => Handle);
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Association_Database.Has: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Has;

   --------------
   --  Insert  --
   --------------

   procedure Insert
     (Item : in AWS.OpenID.Security.Association)
   is
   begin
      Database.Insert (Item => Item);
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Association_Database.Insert: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Insert;

   ------------
   --  Load  --
   ------------

   procedure Load
     (File_Name : in String)
   is
   begin
      Database.Load (File_Name => File_Name);
   end Load;

   ---------------
   --  Look_Up  --
   ---------------

   function Look_Up
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return AWS.OpenID.Security.Association
   is
   begin
      return Database.Look_Up (Handle => Handle);
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Association_Database.Look_Up: " &
              Ada.Exceptions.Exception_Name (E));
         raise;
   end Look_Up;

   ------------
   --  Save  --
   ------------

   procedure Save
     (File_Name : in String)
   is
   begin
      Database.Save (File_Name => File_Name);
   end Save;

end Association_Database;
