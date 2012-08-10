--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Security.OpenID;

package Authentication_Database is
   subtype Authentication_Token is String (1 .. 42);

   procedure Clean_Up;

   Not_Authenticated : exception;

   function Token (Item     : in     Security.OpenID.Authentication;
                   Lifetime : in     Duration) return Authentication_Token;

   function Has (Token : in String) return Boolean;

   function Identity (Token : in String) return String;

   procedure Delete (Token : in     String);
end Authentication_Database;
