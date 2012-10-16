--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Authentication_Database;

package body AWS.OpenID.State is
   procedure Save (File_Name : in     String)
     renames Authentication_Database.Save;
   procedure Load (File_Name : in     String)
     renames Authentication_Database.Load;
end AWS.OpenID.State;
