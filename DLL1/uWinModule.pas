unit uWinModule;

interface

uses
  System.SysUtils,
  WinApi.Windows;

const
  wDir = 'Result';

function GetResultDir: string;

implementation

function GetResultDir: string;
var
  ExePath: array[0..MAX_PATH] of Char;
  Length: DWORD;
begin
  Length := GetModuleFileName(GetModuleHandle(nil), ExePath, MAX_PATH);

  if Length > 0 then
  begin
    result := IncludeTrailingPathDelimiter(ExtractFileDir(ExePath)) + wDir;
  end
  else
    raise Exception.Create('ERROR! Ошибка при получении пути к исполняемому файлу');
end;

end.
