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

package Association_Database is
   procedure Clean_Up;

   procedure Insert (Item : in     Security.OpenID.Association);

   function Has (Handle : in Security.OpenID.Association_Handle) return Boolean;

   function Look_Up (Handle : in Security.OpenID.Association_Handle)
     return Security.OpenID.Association;

   procedure Save (File_Name : in     String);
   procedure Load (File_Name : in     String);
end Association_Database;
