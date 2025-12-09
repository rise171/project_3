unit Config;
{$mode objfpc}{$H+}

interface

function GetEnvDef(const name, def: string): string;

implementation

uses SysUtils;

function GetEnvDef(const name, def: string): string;
var v: string;
begin
  v := GetEnvironmentVariable(name);
  if v = '' then Exit(def) else Exit(v);
end;

end. 