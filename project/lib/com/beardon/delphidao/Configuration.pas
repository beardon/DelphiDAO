{$I .\Defines.inc}
unit Configuration;

interface

const
  // Database connection constants
{$IFDEF DevelopmentDB}
  DB_HOST = 'localhost';
{$ELSEIF TestDB}
  DB_HOST = 'db.test.osufst.org';
{$ELSE}
  DB_HOST = 'db.osufst.org';
{$ENDIF}
  DB_PORT = 3306;
  DB_USERNAME = 'AppUse';
  DB_PASSWORD = 'oldspice';
{$IFDEF ServiceApp}
  DB_USERNAME = 'ServiceUse';
  DB_PASSWORD = 'chunkymonkey';
{$ENDIF}
  DB_SCHEMA = 'schema';

implementation

end.
