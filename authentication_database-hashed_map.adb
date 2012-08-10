--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Characters.Handling,
  Ada.Containers.Indefinite_Hashed_Maps,
  Ada.Exceptions,
  Ada.Numerics.Discrete_Random,
  Ada.Strings.Fixed.Hash;
with
  Yolk.Log;

package body Authentication_Database is
   package Random_Characters is
      new Ada.Numerics.Discrete_Random (Ada.Characters.Handling.ISO_646);

   package Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
           (Key_Type        => Authentication_Token,
            Hash            => Ada.Strings.Fixed.Hash,
            Equivalent_Keys => "=",
            Element_Type    => String,
            "="             => "=");

   protected Database is
      procedure Clean_Up;
      procedure Insert (Identity : in     String;
                        Token    :    out Authentication_Token);
      function Has (Token : in String) return Boolean;
      function Identity (Token : in String) return String;
      procedure Delete (Token : in     String);
   private
      Authentications : Maps.Map := Maps.Empty_Map;
      Token_Generator : Random_Characters.Generator;
   end Database;

   protected body Database is
      procedure Clean_Up is
      begin
         raise Program_Error;
      end Clean_Up;

      procedure Insert (Identity : in     String;
                        Token    :    out Authentication_Token) is
         procedure Generate_Random (Token :    out Authentication_Token) is
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

     Search_For_Unused_Token:
         loop
            Generate_Random (Token);

            if Maps.Contains (Container => Authentications, Key => Token) then
               null;
            else
               Maps.Insert (Container => Authentications,
                            Key       => Token,
                            New_Item  => Identity);
               exit Search_For_Unused_Token;
            end if;
         end loop Search_For_Unused_Token;
      exception
         when others =>
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Failed to insert <" & Identity & "> into the " &
                          "authentication database with the token <" &
                          Token & ">.");
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Current database capacity: " &
                          Maps.Capacity (Authentications)'Img);
            Yolk.Log.Trace
              (Handle  => Yolk.Log.Error,
               Message => "Hash value: " &
                          Ada.Strings.Fixed.Hash (Token)'Img);
            raise;
      end Insert;

      function Has (Token : in String) return Boolean is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Looking up if <" & Token &
                       "> exists in the authentication database.");
         return Maps.Contains (Container => Authentications,
                               Key       => Token);
      end Has;

      function Identity (Token : in String) return String is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Looking <" & Token &
                       "> up in the authentication database.");
         return Maps.Element (Container => Authentications,
                              Key       => Token);
      exception
         when Constraint_Error =>
            if Maps.Contains (Container => Authentications,
                              Key       => Token) then
               raise;
            else
               raise Not_Authenticated;
            end if;
         when others =>
            raise;
      end Identity;

      procedure Delete (Token : in     String) is
      begin
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Info,
            Message => "Removing <" & Token &
                       "> from the authentication database.");
         Maps.Delete (Container => Authentications,
                      Key       => Token);
      exception
         when others =>
            if Maps.Contains (Container => Authentications,
                              Key       => Token) then
               raise;
            end if;
      end Delete;
   end Database;

   function Token (Item     : in     Security.OpenID.Authentication;
                   Lifetime : in     Duration) return Authentication_Token is
      pragma Inline (Token);
   begin
      return Result : Authentication_Token do
        Database.Insert (Identity => Security.OpenID.Identity (Item),
                         Token    => Result);
      end return;
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Token: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Token;

   function Has (Token : in String) return Boolean is
      pragma Inline (Has);
   begin
      return Database.Has (Token => Token);
   exception
      when Constraint_Error =>
         return False;
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Has: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Has;

   function Identity (Token : in String) return String is
      pragma Inline (Identity);
   begin
      return Database.Identity (Token => Token);
   exception
      when Not_Authenticated | Constraint_Error =>
         raise;
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Identity: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Identity;

   procedure Delete (Token : in     String) is
      pragma Inline (Delete);
   begin
      Database.Delete (Token => Token);
   exception
      when E : others =>
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Delete: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Delete;
end Authentication_Database;
