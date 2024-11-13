unit uMain;

interface

uses
  Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls,
  Windows, Vcl.ExtCtrls,
  Vcl.Grids,
  uLog,
  uTypeModule,
  TaskManager;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    PanelChooseTask: TPanel;
    RadioGroupDLL: TRadioGroup;
    Panel1: TPanel;
    Label1: TLabel;
    ComboBoxTasks: TComboBox;
    Panel2: TPanel;
    Label2: TLabel;
    LabeledEditPath: TLabeledEdit;
    Memo2: TMemo;
    LabeledEditArcSrc: TLabeledEdit;
    LabeledEditArcDst: TLabeledEdit;
    Start: TButton;
    Panel3: TPanel;
    StringGridTask: TStringGrid;
    Label3: TLabel;
    Memo1: TMemo;
    procedure StartClick(Sender: TObject);
    procedure RadioGroupDLLClick(Sender: TObject);
    procedure ComboBoxTasksChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGridTaskSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    FID,
    FStartID: Integer;
    FStartDir,
    FFilePath,
    FCommandLine,
    FCommandSrc,
    FCommandDst: string;
    FListParamMask,
    FListParamText: TStrings;
    FRadioGroupItem: Integer;
    FTaskManager: TTaskManager;
    procedure UpdateInfo;
    function NextID: Integer;
    procedure ProgressStatus(ID, Progress: Integer; Status: PAnsiChar); stdcall;
    procedure StartTaskArc(ID: Integer);
    procedure StartTaskFind(ID: Integer; Data: TStrings);


  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure status(ID, Progress: Integer; Status: PAnsiChar); stdcall;
begin
  form1.ProgressStatus(ID, Progress, Status)
end;

procedure TForm1.StartClick(Sender: TObject);
var
  id: Integer;
  taskName: String;
  Data: TStrings;
begin

  if Length(LabeledEditPath.Text)<3 then
    raise Exception.Create('Проверьте введённые данные');

  id := NextID;

  case RadioGroupDLL.ItemIndex of
    1:        // lib2.dll
    begin
      taskName := 'Lib2_Arc_'+id.ToString;

      FTaskManager.AddTask(id,taskName,
        procedure
        begin
          ToLog(id.ToString, 'Task started.');
          StartTaskArc(id);
          ToLog(id.ToString, 'Task completed.');
        end);

      FTaskManager.StartTask(id);

    end;
    else begin
      Data := TStringList.Create;

      Data := TStringList.Create;
      Data.Add(form1.LabeledEditPath.Text);
      Data.AddStrings(Memo2.Lines);

      taskName := 'Lib1_';
      case form1.ComboBoxTasks.ItemIndex of
        1: taskName := taskName + 'FindText_';
        else taskName := taskName + 'FindFile_';
      end;

      taskName := taskName + id.ToString;

      FTaskManager.AddTask(id,taskName,
        procedure
        begin
          ToLog(id.ToString, 'Task started.');
          //r := GetTasks(2, @Masks, @Lns, n);
          StartTaskFind(id, Data);      // lib1.dll
          ToLog(id.ToString, 'Task completed.');
        end);

      FTaskManager.StartTask(id);

    end;
  end; //case RadioGroupDLL.ItemIndex of

  UpdateInfo;

end;

procedure TForm1.ComboBoxTasksChange(Sender: TObject);
begin
  Memo2.Clear;

  case form1.RadioGroupDLL.ItemIndex of
   1:
   begin
      Memo2.Visible := false;
      LabeledEditPath.Visible := true;
      LabeledEditPath.Enabled := false;

      LabeledEditPath.Text := FCommandLine;
      LabeledEditArcSrc.Text := FCommandSrc;
      LabeledEditArcDst.Text := FCommandDst;

      LabeledEditArcSrc.Visible := true;
      LabeledEditArcDst.Visible := true;

   end;
   else begin
      Memo2.Visible := true;
      LabeledEditPath.Visible := true;
      LabeledEditPath.Enabled := true;
      LabeledEditArcSrc.Visible := false;
      LabeledEditArcDst.Visible := false;

      //заполним форму ранее использованными данными:
      if form1.ComboBoxTasks.ItemIndex<1
        then begin
          LabeledEditPath.EditLabel.Caption := 'Введите директорию для поиска:';
          LabeledEditPath.Text := FStartDir;
          Memo2.Lines.AddStrings(FListParamMask);
        end
        else begin
          LabeledEditPath.EditLabel.Caption := 'Введите путь и имя файла с данными:';
          LabeledEditPath.Text := FFilePath;
          Memo2.Lines.AddStrings(FListParamText);

        end;

   end;
  end;
end;

constructor TForm1.Create(AOwner: TComponent);
var
  FilePath: string;
begin
  inherited Create(AOwner);

  //зададим начальное значение для FID:
  FID := 0;
  repeat
    inc(FID);
    FilePath := IncludeTrailingPathDelimiter('Result') + FID.ToString + '.txt';
  until (not FileExists(FilePath));
  dec(FID);
  FStartID:=FID;

  FListParamMask := TStringList.Create;
  FListParamText := TStringList.Create;

  FTaskManager := TTaskManager.Create;

  //путь для запуска архивации:
  //FCommandLine := 'C:\Program Files\7z.exe';
  //FCommandLine := 'C:\Program Files (x86)\WinRAR\WinRAR.exe';
  FCommandLine := 'C:\Program Files (x86)\WinRAR\Rar.exe';

  //для теста
  FStartDir:='D:\Projects\Delphi\TestAndSobes\01';
  FListParamMask.Add('*.txt');

  FFilePath := 'D:\Projects\Delphi\TestAndSobes\MainPr\Win32\Debug\test.txt';
  FListParamText.Add('bar');
  FListParamText.Add('win');
  FListParamText.Add('end');
  FListParamText.Add('END');
  FListParamText.Add('WIN');

  FCommandSrc := 'D:\Projects\Delphi\TestAndSobes\01\55';
  FCommandDst := 'D:\Projects\Delphi\TestAndSobes\01\23.rar';

  //----для теста----//

  // Устанавливаем заголовки для StringGrid1
  StringGridTask.Cells[0, 0] := 'ID';
  StringGridTask.Cells[1, 0] := 'Name';
  StringGridTask.Cells[2, 0] := 'Status';

  StringGridTask.ColWidths[0] := 50;
  StringGridTask.ColWidths[1] := 150;
  StringGridTask.ColWidths[2] := 200;

  StringGridTask.RowCount := 2;

end;

destructor TForm1.Destroy;
begin
  FTaskManager.Destroy;
  FListParamMask.Free;
  FListParamText.Free;
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  LabeledEditPath.Top := 40;
  LabeledEditArcSrc.Top := 90;
  LabeledEditArcDst.Top := 140;
  Memo2.Top := 75;
  Memo2.Height := 190;

  PanelChooseTask.Height := 450;

  RadioGroupDLL.ItemIndex := 0;
  ComboBoxTasks.ItemIndex := 0;
  ComboBoxTasksChange(Sender);

end;

function TForm1.NextID: Integer;
begin
  Inc(FID);
  Result := FID;
end;

procedure TForm1.ProgressStatus(ID, Progress: Integer; Status: PAnsiChar); stdcall;
begin
  StatusBar1.Panels[0].Text := 'Задача: ' + ID.ToString;
  StatusBar1.Panels[1].Text := 'Статус: ' + Format('Progress: %d%% - %s', [Progress, Status]);
end;

procedure TForm1.RadioGroupDLLClick(Sender: TObject);
begin

 if FRadioGroupItem <> RadioGroupDLL.ItemIndex
  then begin
    FRadioGroupItem := RadioGroupDLL.ItemIndex;
    case RadioGroupDLL.ItemIndex of
      1:
       begin
         ComboBoxTasks.Clear;
         ComboBoxTasks.Items.Add('Архивирование...')
       end;
       else begin
         ComboBoxTasks.Clear;
         ComboBoxTasks.Items.Add('Поиск файлов (FindFiles)');
         ComboBoxTasks.Items.Add('Поиск вхождений (FindText)')
       end;
     end;
    ComboBoxTasks.ItemIndex := 0;
    ComboBoxTasksChange(Sender);
  end;

end;

procedure TForm1.StartTaskArc(ID: Integer);
type
  TGetArcProc = procedure(ID: Integer; AppPath, SourceDirectory, OutputArchive: PAnsiChar; Callback: TProgressCallback); stdcall;
var
  dllH: THandle;
  GetArc: TGetArcProc;

  cmdLine,
  cmdParSrc,
  cmdParDst:  PAnsiChar;

  Callback: TProgressCallback;
begin

  dllH := LoadLibrary('lib2.dll');

  if dllH = 0 then
      raise Exception.Create('Could not load DLL');

  @GetArc := GetProcAddress(dllH, 'CreateZipArchive');

  if not assigned(GetArc) then
      raise exception.Create('[Неудачный вызов GetProcAddress]');

  try

    // заполняем параметры
    FCommandSrc := LabeledEditArcSrc.Text;
    FCommandDst := LabeledEditArcDst.Text;

    cmdLine := AnsiStrAlloc(FCommandLine.Length + 1);
    StrPCopy(cmdLine, AnsiString(FCommandLine));
    cmdParSrc := AnsiStrAlloc(FCommandSrc.Length + 1);
    StrPCopy(cmdParSrc, AnsiString(FCommandSrc));
    cmdParDst := AnsiStrAlloc(FCommandDst.Length + 1);
    StrPCopy(cmdParDst, AnsiString(FCommandDst));

    callback := status;

    { Теперь выполняем задачу  }
    GetArc(id,cmdLine,cmdParSrc,cmdParDst,callback);

  finally
    { Освобождаем дескриптор DLL }
    FreeLibrary(dllH);
    strDispose(cmdLine);
    strDispose(cmdParSrc);
    strDispose(cmdParDst);
  end;

end;


procedure TForm1.StartTaskFind(ID: Integer; Data: TStrings);
type
  TGetTasksProc = function(ID: Integer; Data: PPAnsiCharArray; PLengths: PIntegerArray; Count: Integer): Integer; stdcall;
var
  dllH: THandle;
  GetTasks: TGetTasksProc;
  i,n,r: Integer;
  str: String;
  Masks: array of PAnsiChar;
  Lns: array of Integer;

begin
  dllH := LoadLibrary('lib1.dll');

  if dllH = 0 then
      raise Exception.Create('Could not load DLL');

  case form1.ComboBoxTasks.ItemIndex of
    1: @GetTasks := GetProcAddress(dllH, 'FindTextOccurrences');
    else @GetTasks := GetProcAddress(dllH, 'FindFiles');

  end;

  if not assigned(GetTasks) then
    raise exception.Create('[Неудачный вызов GetProcAddress]');


  try
    // вносим параметры
    n := Data.Count;
    SetLength(Masks, n);
    SetLength(Lns, n);

    for i := 0 to n-1 do
      begin
          str := Data[i];
          Lns[i] := str.Length;
          Masks[i] := AnsiStrAlloc(Lns[i] + 1); // +1 для нулевого символа
          StrPCopy(Masks[i], AnsiString(str));
      end;

    Data.Free;
    { Теперь выполняем функцию  }

    r := GetTasks(id, @Masks, @Lns, n);

    //showMessage('Количество файлов = ' + r.ToString)
  finally
    { Освобождаем дескриптор DLL }
    FreeLibrary(dllH);
    for i := 0 to n-1 do strDispose(Masks[i]);

  end;

end;

procedure TForm1.StringGridTaskSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
  FilePath: string;
begin
  if ARow<1 then Exit;

  FilePath := IncludeTrailingPathDelimiter('Result') + StringGridTask.Cells[0, ARow] + '.txt';


  Memo1.Clear;
  if FileExists(FilePath) then
  begin
    Memo1.Lines.LoadFromFile(FilePath, TEncoding.UTF8);
  end
  else begin
    Memo1.Lines.Add('Файл "' + FilePath + '" не найден.');
  end;

end;

procedure TForm1.UpdateInfo;
var
  FTask: TLocalTask;
  i,j: Integer;
begin
  //StatusBar1.Panels[0].Text := 'Задача: -';
  //StatusBar1.Panels[1].Text := 'Статус: ';

  if FTaskManager.Count<1 then
    Exit;


  StringGridTask.RowCount := FTaskManager.Count + 1;

  i:=0;

  //for i := 0 to FTaskManager.Count-1 do
    //begin
      for j := FStartID to FID do
        begin
          FTask := FTaskManager.FindTaskByID(j);
          if Assigned(FTask)
            then begin
              StringGridTask.Cells[0, i + 1] := FTask.ID.ToString;//(i + 1); // id задачи
              StringGridTask.Cells[1, i + 1] := FTask.Name; // имя задачи
              StringGridTask.Cells[2, i + 1] := FTask.StringStatus;
              inc(i);
            end;
        end;
    //end;

            {
      if FTask <> nil
        then begin

          StringGridTask.Cells[2, i + 1] := FTask.Status; // статус задачи
        end
        else begin
          StringGridTask.Cells[0, i + 1] := IntToStr(i + 1); // id задачи
          StringGridTask.Cells[1, i + 1] := ' - '; // имя задачи
          StringGridTask.Cells[2, i + 1] := 'Завершена'; // статус задачи
        end;
        }

  Panel3.Visible := true;

end;


end.
