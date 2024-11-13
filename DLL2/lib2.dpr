library lib2;
//архивирование данных в zip

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  uTypeModule in '..\Common\uTypeModule.pas',
  uWinModule in '..\Common\uWinModule.pas',
  uLog in '..\Common\uLog.pas';

function GetCurrentProgress: Integer;
begin
  Result := 0; //заглушка для функции прогресса
end;

procedure CreateZipArchive(ID: Integer; AppPath, SourceDirectory, OutputArchive: PAnsiChar; Callback: TProgressCallback); stdcall;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  ExitCode: DWORD;
  CommandLine: string;
  Progress: Integer;
  resultFN: string;
begin
  //AppPath := 'C:\Program Files\7-Zip\7z.exe'; // Путь к 7-Zip
  //CommandLine := Format('"%s" a "%s" "%s\*" -r', [AppPath, OutputArchive, SourceDirectory]);

  CommandLine := Format('"%s" a -ep1 "%s" "%s\*"', [AppPath, OutputArchive, SourceDirectory]);

  resultFN := ID.ToString;
  ToLog(resultFN,'Запуск приложения: '+#13#10+CommandLine, false);
  //CommandLine := Format('"%s"', ['C:\Program Files (x86)\WinRAR\Rar.exe x "D:\Projects\Delphi\TestAndSobes\01\1.rar" "D:\Projects\Delphi\TestAndSobes\01\55"']);

  // устанавливаем атрибуты процесса
  ZeroMemory(@si, SizeOf(si));
  si.cb := SizeOf(si);

  if CreateProcess(nil, PChar(CommandLine), nil, nil, False, CREATE_NO_WINDOW, nil, nil, si, pi) then
  begin
    try
      ToLog(resultFN,'Старт процесса архивирования ');
      repeat
        sleep(500);
        Progress := GetCurrentProgress(); // Пока делаю заглушку для функции, предоставляющей прогресс
        if Assigned(Callback) then
          Callback(ID, Progress, 'Running...');
      until WaitForSingleObject(pi.hProcess, 500) <> WAIT_TIMEOUT;

      GetExitCodeProcess(pi.hProcess, ExitCode);
      ToLog(resultFN,'Завершение процесса... ');

      if ExitCode = 0 then
      begin
        if Assigned(Callback) then
          Callback(ID, 100, 'Completed');
        ToLog(resultFN,'Процесс завершён.');
      end
      else
      begin
        if Assigned(Callback) then
          Callback(ID, 0, 'Error');
          ToLog(resultFN,'Ошибка при завершении процесса');
      end;
    finally
      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    end;
  end
  else
  begin
    if Assigned(Callback) then
      Callback(ID, 0, 'Failed to start process');
    ToLog(resultFN,'Ошибка при старте процесса архивирования', false);
  end;
end;


exports
  CreateZipArchive;

begin
end.
