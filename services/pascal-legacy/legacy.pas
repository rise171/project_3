program LegacyCSV;
{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, Process, BaseUnix,
  Config, CSVGen;

function EnsureDir(const p: string): Boolean;
begin
  if not DirectoryExists(p) then
    Result := CreateDir(p)
  else
    Result := True;
end;

function ImportToPg(const fullpath: string): integer;
var pghost, pgport, pguser, pgpass, pgdb: string;
    proc: TProcess;
begin
  pghost := GetEnvDef('PGHOST', 'db');
  pgport := GetEnvDef('PGPORT', '5432');
  pguser := GetEnvDef('PGUSER', 'monouser');
  pgpass := GetEnvDef('PGPASSWORD', 'monopass');
  pgdb   := GetEnvDef('PGDATABASE', 'monolith');

  SetEnvironmentVariable('PGPASSWORD', pgpass);

  proc := TProcess.Create(nil);
  try
    proc.Executable := 'psql';
    proc.Parameters.Add(Format(
      'host=%s port=%s user=%s dbname=%s',
      [pghost, pgport, pguser, pgdb]
    ));
    proc.Parameters.Add('-c');
    proc.Parameters.Add(
      '\copy telemetry_legacy(recorded_at,voltage,temp,source_file)' +
      ' FROM STDIN WITH (FORMAT csv, HEADER true)'
    );
    proc.Options := [poUsePipes];
    proc.Execute;

    with proc.Input do begin
      LoadFromFile(fullpath);
      Close;
    end;

    proc.WaitOnExit;
    Result := proc.ExitStatus;
  finally
    proc.Free;
  end;
end;

var
  outDir, fn, fullpath: string;
  ts: string;
  period: Integer;
begin
  Randomize;
  period := StrToIntDef(GetEnvDef('GEN_PERIOD_SEC', '300'), 300);

  outDir := GetEnvDef('CSV_OUT_DIR', '/data/csv');
  if not EnsureDir(outDir) then
    raise Exception.Create('Cannot access CSV_OUT_DIR: ' + outDir);

  WriteLn('[legacy] started');

  while True do
  begin
    ts := FormatDateTime('yyyymmdd_hhnnss', Now);
    fn := 'telemetry_' + ts + '.csv';
    fullpath := IncludeTrailingPathDelimiter(outDir) + fn;

    CSVGen.GenerateCSVFile(fullpath);

    if ImportToPg(fullpath) <> 0 then
      WriteLn('[legacy] import failed: ', fullpath)
      WriteLn('[legacy] COPY into postgres...')
    else
      WriteLn('[legacy] imported: ', fullpath);

    fpSelect(0, nil, nil, nil, period * 1000);
  end;
end.
