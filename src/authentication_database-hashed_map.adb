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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed.Hash;

with AWS.Cookie;

with AWS.OpenID.Log;

package body Authentication_Database is

   Token_Lifetime    : constant Duration := 3600.0;
   Token_Cookie_Name : constant String := "token";

   subtype Authentication_Token is String (1 .. 42);

   package Random_Characters is new
     Ada.Numerics.Discrete_Random (Ada.Characters.Handling.ISO_646);

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Authentication_Token,
      Hash            => Ada.Strings.Fixed.Hash,
      Equivalent_Keys => "=",
      Element_Type    => String,
      "="             => "=");

   protected Database is
      procedure Delete
        (Token : in String);
      --  TODO: write comment

      function Has
        (Token : in String)
         return Boolean;
      --  TODO: write comment

      function Identity
        (Token : in String)
         return String;
      --  TODO: write comment

      procedure Insert
        (Identity : in     String;
         Token    :    out Authentication_Token);
      --  TODO: write comment.
   private
      Authentications : Maps.Map := Maps.Empty_Map;
      Token_Generator : Random_Characters.Generator;
   end Database;
   --  TODO: Write comment

   protected body Database is

      --------------
      --  Delete  --
      --------------

      procedure Delete
        (Token : in String)
      is
         use AWS.OpenID;
      begin
         Log.Info
           (Message => "Removing <" & Token &
              "> from the authentication database.");

         Authentications.Delete (Token);
      exception
         when others =>
            if Authentications.Contains (Token) then
               raise;
            end if;
      end Delete;

      -----------
      --  Has  --
      -----------

      function Has
        (Token : in String)
         return Boolean
      is
         use AWS.OpenID;
      begin
         Log.Info ("Looking up if <" & Token &
                     "> exists in the authentication database.");

         return Authentications.Contains (Token);
      end Has;

      ----------------
      --  Identity  --
      ----------------

      function Identity
        (Token : in String)
         return String
      is
         use AWS.OpenID;
      begin
         Log.Info ("Looking <" & Token &
                     "> up in the authentication database.");

         return Authentications.Element (Token);
      exception
         when Constraint_Error =>
            if Authentications.Contains (Token) then
               raise;
            else
               raise Not_Authenticated;
            end if;
      end Identity;

      --------------
      --  Insert  --
      --------------

      procedure Insert
        (Identity : in     String;
         Token    :    out Authentication_Token)
      is
         use AWS.OpenID;

         procedure Generate_Random
           (Token : out Authentication_Token);
         --  TODO: write comment

         -----------------------
         --  Generate_Random  --
         -----------------------

         procedure Generate_Random
           (Token : out Authentication_Token)
         is
            use Ada.Characters.Handling;
         begin
            for Index in Token'Range loop
               loop
                  Token (Index) := Random_Characters.Random (Token_Generator);

                  exit when Is_Letter (Token (Index)) or
                    Is_Digit (Token (Index));
               end loop;
            end loop;
         end Generate_Random;
      begin
         Search_For_Unused_Token :
         loop
            Generate_Random (Token);

            if not Authentications.Contains (Token) then
               Authentications.Insert (Token, Identity);

               exit Search_For_Unused_Token;
            end if;
         end loop Search_For_Unused_Token;
      exception
         when others =>
            Log.Error ("Failed to insert <" & Identity & "> into the " &
                         "authentication database with the token <" &
                         Token & ">.");
            Log.Error ("Current database capacity: " &
                         Authentications.Capacity'Img);
            Log.Error ("Hash value: " &
                         Ada.Strings.Fixed.Hash (Token)'Img);

            raise;
      end Insert;

   end Database;

   -----------------------
   --  Delete_Identity  --
   -----------------------

   procedure Delete_Identity
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      use Ada.Exceptions;
      use AWS.OpenID;

      Token : Authentication_Token;
   begin
      Token := AWS.Cookie.Get (Request => Request,
                               Key     => Token_Cookie_Name);

      Database.Delete (Token);

      AWS.Cookie.Expire (Content => Response,
                         Key     => Token_Cookie_Name);
   exception
      when E : others =>
         Log.Error ("Exception in Authentication_Database.Delete: " &
                      Exception_Name (E));
         raise;
   end Delete_Identity;

   ----------------
   --  Identity  --
   ----------------

   function Identity
     (Request : in AWS.Status.Data)
      return String
   is
      use Ada.Exceptions;
      use AWS.OpenID;
   begin
      return Database.Identity (AWS.Cookie.Get (Request, Token_Cookie_Name));
   exception
      when Not_Authenticated | Constraint_Error =>
         raise;
      when E : others =>
         Log.Error ("Exception in Authentication_Database.Identity: " &
                      Exception_Name (E));
         raise;
   end Identity;

   ------------------------
   --  Is_Authenticated  --
   ------------------------

   function Is_Authenticated
     (Request  : in AWS.Status.Data)
      return Boolean
   is
      use Ada.Exceptions;
      use AWS.OpenID;
   begin
      return Database.Has (AWS.Cookie.Get (Request, Token_Cookie_Name));
   exception
      when Constraint_Error =>
         return False;
      when E : others =>
         Log.Error ("Exception in Authentication_Database." &
                      "Is_Authenticated: " &
                      Exception_Name (E) & " (" &
                      Exception_Information (E) & ")");
         raise;
   end Is_Authenticated;

   ------------
   --  Load  --
   ------------

   procedure Load
     (File_Name : in String)
   is
   begin
      null;
   end Load;

   -------------------------
   --  Register_Identity  --
   -------------------------

   procedure Register_Identity
     (Source   : in     AWS.OpenID.Security.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      pragma Unreferenced (Request);

      use Ada.Exceptions;
      use AWS.OpenID;

      Token : Authentication_Token;
   begin
      if Security.Authenticated (Source) then
         Database.Insert (Security.Identity (Source), Token);

         AWS.Cookie.Set (Content => Response,
                         Key     => Token_Cookie_Name,
                         Value   => Token,
                         Max_Age => Token_Lifetime);
      else
         raise Not_Authenticated;
      end if;
   exception
      when Not_Authenticated =>
         raise;
      when E : others =>
         Log.Error ("Exception in Authentication_Database." &
                      "Register_Identity: " &
                      Exception_Name (E) & " (" &
                      Exception_Information (E) & ")");
         raise;
   end Register_Identity;

   ------------
   --  Save  --
   ------------

   procedure Save
     (File_Name : in String)
   is
   begin
      null;
   end Save;

end Authentication_Database;
