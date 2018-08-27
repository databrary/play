; -- last migration missing trailing semicolon
update account set password = null where email = 'lisa@databrary.org';
 
