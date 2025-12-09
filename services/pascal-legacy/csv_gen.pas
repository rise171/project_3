unit CSVGen;
{$mode objfpc}{$H+}

interface

procedure GenerateCSVFile(const fullpath: string);

implementation

uses SysUtils, DateUtils, Config;

function RandFloat(minV, maxV: Double): Double;
begin
  Result := minV + Random * (maxV - minV);
end;

procedure GenerateCSVFile(const fullpath: string);
var f: TextFile;
begin
  AssignFile(f, fullpath);
  Rewrite(f);
  Writeln(f, 'recorded_at,voltage,temp,source_file');
  Writeln(f,
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ',' +
    FormatFloat('0.00', RandFloat(3.2, 12.6)) + ',' +
    FormatFloat('0.00', RandFloat(-50.0, 80.0)) + ',' +
    ExtractFileName(fullpath)
  );
  CloseFile(f);
end;

end.
