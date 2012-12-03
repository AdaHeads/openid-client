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

with
  Ada.Containers.Hashed_Maps,
  Ada.Exceptions,
  Ada.Streams.Stream_IO,
  Ada.Strings.Unbounded,
  Ada.Strings.Unbounded.Hash;
with
  Yolk.Log;

package body Association_Database is
   package Maps is
     new Ada.Containers.Hashed_Maps
           (Key_Type        => Security.OpenID.Association_Handle,
            Hash            => Ada.Strings.Unbounded.Hash,
            Equivalent_Keys => Ada.Strings.Unbounded."=",
            Element_Type    => Security.OpenID.Association,
            "="             => Security.OpenID."=");

   protected Database is
      procedure Clean_Up;
      procedure Insert (Item : in     Security.OpenID.Association);
      function Has (Handle : in Security.OpenID.Association_Handle)
        return Boolean;
      function Look_Up (Handle : in Security.OpenID.Association_Handle)
        return Security.OpenID.Association;
      procedure Save (File_Name : in     String);
      procedure Load (File_Name : in     String);
   private
      Associations : Maps.Map := Maps.Empty_Map;
   end Database;

   protected body Database is
      procedure Clean_Up is
      begin
         raise Program_Error;
      end Clean_Up;

      procedure Insert (Item : in     Security.OpenID.Association) is
         Key : constant Security.OpenID.Association_Handle
                 := Security.OpenID.Handle (Item);
      begin
         Maps.Insert (Container => Associations,
                      Key       => Key,
                      New_Item  => Item);
      exception
         when others =>
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Failed to insert <" &
                          Security.OpenID.To_String (Item) &
                          "> into the association database with the handle <" &
                          Ada.Strings.Unbounded.To_String (Key) & ">.");
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Current database capacity: " &
                          Maps.Capacity (Associations)'Img);
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Hash value: " &
                          Ada.Strings.Unbounded.Hash (Key)'Img);
            raise;
      end Insert;

      function Has (Handle : in Security.OpenID.Association_Handle)
        return Boolean is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Looking up if <" &
                       Ada.Strings.Unbounded.To_String (Handle) &
                       "> exists in the association database.");
         return Maps.Contains (Container => Associations,
                               Key       => Handle);
      end Has;

      function Look_Up (Handle : in Security.OpenID.Association_Handle)
        return Security.OpenID.Association is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Looking <" & Ada.Strings.Unbounded.To_String (Handle) &
                       "> up in the association database.");
         return Maps.Element (Container => Associations,
                              Key       => Handle);
      end Look_Up;

      procedure Save (File_Name : in     String) is
         use Ada.Streams.Stream_IO;
         File    : Ada.Streams.Stream_IO.File_Type;
         Target  : Ada.Streams.Stream_IO.Stream_Access;

         procedure Save (Position : in     Maps.Cursor) is
         begin
            Security.OpenID.Association_Handle'Output (Target,
                                                       Maps.Key (Position));
            Security.OpenID.Association'Output (Target,
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

      procedure Load (File_Name : in     String) is
         use Ada.Streams.Stream_IO;
         File    : Ada.Streams.Stream_IO.File_Type;
         Source  : Ada.Streams.Stream_IO.Stream_Access;
         Key     : Security.OpenID.Association_Handle;
         Element : Security.OpenID.Association;
      begin
         Open (File => File,
               Name => File_Name,
               Mode => In_File);
         Source := Stream (File);
         while not End_Of_File (File) loop
            Key     := Security.OpenID.Association_Handle'Input (Source);
            Element := Security.OpenID.Association'Input (Source);
            Maps.Insert (Container => Associations,
                         Key       => Key,
                         New_Item  => Element);
         end loop;
         Close (File => File);
      end Load;
   end Database;

   procedure Clean_Up is
      pragma Inline (Clean_Up);
   begin
      Database.Clean_Up;
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Clean_Up: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Clean_Up;

   procedure Insert (Item : in     Security.OpenID.Association) is
      pragma Inline (Insert);
   begin
      Database.Insert (Item => Item);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Insert: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Insert;

   function Has (Handle : in Security.OpenID.Association_Handle)
     return Boolean is
      pragma Inline (Has);
   begin
      return Database.Has (Handle => Handle);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Has: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Has;

   function Look_Up (Handle : in Security.OpenID.Association_Handle)
     return Security.OpenID.Association is
      pragma Inline (Look_Up);
   begin
      return Database.Look_Up (Handle => Handle);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Association_Database.Look_Up: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Look_Up;

   procedure Save (File_Name : in     String) is
   begin
      Database.Save (File_Name => File_Name);
   end Save;

   procedure Load (File_Name : in     String) is
   begin
      Database.Load (File_Name => File_Name);
   end Load;
end Association_Database;
