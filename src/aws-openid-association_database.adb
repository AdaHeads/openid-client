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
with Ada.Text_IO;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with AWS.OpenID.Log;

package body AWS.OpenID.Association_Database is

   package Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => AWS.OpenID.Security.Association_Handle,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      Element_Type    => AWS.OpenID.Security.Association,
      "="             => AWS.OpenID.Security."=");

   package Stale_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => AWS.OpenID.Security.Association_Handle,
      "="          => Ada.Strings.Unbounded."=");

   Stale_Associations : Stale_List.List;

   protected Database is
      procedure Clean_Up;
      --  Delete all expired Associations.

      function Has
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return Boolean;
      --  Return True if Handle is in the associations database.

      procedure Insert
        (Item : in AWS.OpenID.Security.Association);
      --  Insert Item into the associations database.

      procedure Load
        (File_Name : in String);
      --  Load File_Name into the associations database.

      function Look_Up
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return AWS.OpenID.Security.Association;
      --  Reuturn the Handle association.

      procedure Save
        (File_Name : in String);
      --  Save the association database to File_Name
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
         use AWS.OpenID.Security;
      begin
         for C in Associations.Iterate loop
            if not Is_Expired (Maps.Element (C)) then
               Stale_Associations.Append (Maps.Key (C));
            end if;
         end loop;

         for Handle of Stale_Associations loop
            Ada.Text_IO.Put_Line (Ada.Strings.Unbounded.To_String (Handle));
            Associations.Exclude (Handle);
         end loop;
      end Clean_Up;

      -----------
      --  Has  --
      -----------

      function Has
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return Boolean
      is
         use Ada.Strings.Unbounded;
         use AWS.OpenID;
      begin
         Log.Info ("Looking up if <" &
                     To_String (Handle) &
                     "> exists in the association database.");

         return Associations.Contains (Handle);
      end Has;

      --------------
      --  Insert  --
      --------------

      procedure Insert
        (Item : in AWS.OpenID.Security.Association)
      is
         use Ada.Strings.Unbounded;
         use AWS.OpenID;

         Key : constant Security.Association_Handle := Security.Handle (Item);
      begin
         Associations.Insert (Key, Item);
      exception
         when others =>
            Log.Error ("Failed to insert <" &
                 Security.To_String (Item) &
                 "> into the association database with the handle <" &
                 To_String (Key) & ">.");
            Log.Error ("Current database capacity: " &
                         Associations.Capacity'Img);
            Log.Error ("Hash value: " & Hash (Key)'Img);
            raise;
      end Insert;

      ------------
      --  Load  --
      ------------

      procedure Load
        (File_Name : in String)
      is
         use Ada.Streams.Stream_IO;
         use AWS.OpenID.Security;

         File    : File_Type;
         Source  : Stream_Access;
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

            if not Is_Expired (Element) then
               Associations.Insert (Key, Element);
            end if;
         end loop;

         Close (File);
      end Load;

      ---------------
      --  Look_Up  --
      ---------------

      function Look_Up
        (Handle : in AWS.OpenID.Security.Association_Handle)
         return AWS.OpenID.Security.Association
      is
         use Ada.Strings.Unbounded;
         use AWS.OpenID;
      begin
         Log.Info ("Looking <" &
                     To_String (Handle) &
                     "> up in the association database.");

         return Associations.Element (Handle);
      end Look_Up;

      ------------
      --  Save  --
      ------------

      procedure Save
        (File_Name : in String)
      is
         use Ada.Streams.Stream_IO;

         procedure Save
           (Position : in Maps.Cursor);

         File    : File_Type;
         Target  : Stream_Access;

         ------------
         --  Save  --
         ------------

         procedure Save
           (Position : in Maps.Cursor)
         is
            use AWS.OpenID.Security;
         begin
            if not Is_Expired (Maps.Element (Position)) then
               AWS.OpenID.Security.Association_Handle'Output
                 (Target, Maps.Key (Position));

               AWS.OpenID.Security.Association'Output
                 (Target, Maps.Element (Position));
            end if;
         end Save;
      begin
         Create (File => File,
                 Name => File_Name);
         Target := Stream (File);

         Associations.Iterate (Process => Save'Access);

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
   end Clean_Up;

   ------------------
   --  Has_Handle  --
   ------------------

   function Has_Handle
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return Boolean
   is
      use Ada.Exceptions;
      use AWS.OpenID;
   begin
      return Database.Has (Handle);
   exception
      when E : others =>
         Log.Error ("Exception in Association_Database.Has: " &
                      Exception_Name (E));
         raise;
   end Has_Handle;

   --------------------------
   --  Insert_Association  --
   --------------------------

   procedure Insert_Association
     (Item : in AWS.OpenID.Security.Association)
   is
      use Ada.Exceptions;
      use AWS.OpenID;
   begin
      Database.Insert (Item);
   exception
      when E : others =>
         Log.Error ("Exception in Association_Database.Insert: " &
                      Exception_Name (E) & " (" &
                      Exception_Information (E) & ")");
         raise;
   end Insert_Association;

   ------------
   --  Load  --
   ------------

   procedure Load
     (File_Name : in String)
   is
   begin
      Database.Load (File_Name);
   end Load;

   ----------------------------------
   --  Look_Up_Association_Handle  --
   ----------------------------------

   function Look_Up_Association_Handle
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return AWS.OpenID.Security.Association
   is
      use Ada.Exceptions;
      use AWS.OpenID;
   begin
      return Database.Look_Up (Handle);
   exception
      when E : others =>
         Log.Error ("Exception in Association_Database.Look_Up: " &
                      Exception_Name (E));
         raise;
   end Look_Up_Association_Handle;

   ------------
   --  Save  --
   ------------

   procedure Save
     (File_Name : in String)
   is
   begin
      Database.Save (File_Name);
   end Save;

end AWS.OpenID.Association_Database;
