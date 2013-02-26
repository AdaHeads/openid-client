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
     (Key_Type        => Security.OpenID.Association_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      Element_Type    => Security.OpenID.Association,
      "="             => Security.OpenID."=");

   protected Database is
      procedure Clean_Up;

      function Has
        (Handle : in Security.Openid.Association_Handle)
         return Boolean;

      procedure Insert
        (Item : in Security.Openid.Association);

      procedure Load
        (File_Name : in String);

      function Look_Up
        (Handle : in Security.Openid.Association_Handle)
         return Security.Openid.Association;

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
        (Handle : in Security.Openid.Association_Handle)
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
        (Item : in Security.Openid.Association)
      is
         Key : constant Security.Openid.Association_Handle
           := Security.Openid.Handle (Item);
      begin
         Maps.Insert (Container => Associations,
                      Key       => Key,
                      New_Item  => Item);
      exception
         when others =>
            AWS.OpenID.Log.Error
              (Message => "Failed to insert <" &
                          Security.Openid.To_String (Item) &
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
         Key     : Security.Openid.Association_Handle;
         Element : Security.Openid.Association;
      begin
         Open (File => File,
               Name => File_Name,
               Mode => In_File);
         Source := Stream (File);

         while not End_Of_File (File) loop
            Key     := Security.Openid.Association_Handle'Input (Source);
            Element := Security.Openid.Association'Input (Source);
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
        (Handle : in Security.Openid.Association_Handle)
         return Security.Openid.Association
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
            Security.Openid.Association_Handle'Output (Target,
                                                       Maps.Key (Position));
            Security.Openid.Association'Output (Target,
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
     (Handle : in Security.Openid.Association_Handle)
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
     (Item : in Security.Openid.Association)
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
     (Handle : in Security.Openid.Association_Handle)
      return Security.Openid.Association
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
