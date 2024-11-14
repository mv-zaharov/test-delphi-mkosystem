unit TaskManager;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections,
  Windows, PsAPI;

type
  TTaskEvent = procedure of object;

  TLocalTask = class
  private
    FTask: ITask;
    FStopOnComplete: Boolean;
    FShouldRun: Boolean;
    FCompleting: Boolean;
    FLock: TObject;
    FName: string;
    FID: Integer;
    FOnTaskStop: TTaskEvent;
    function GetStatus: string;
  public
    constructor Create(const ID: integer; const AName: string; const ATaskProcedure: TProc);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
    property Name: string read FName;
    property ID: Integer read FID;
    property StringStatus: string read GetStatus;
    property StopOnComplete: boolean read FStopOnComplete write FStopOnComplete;
    property OnTaskStop: TTaskEvent read FOnTaskStop write FOnTaskStop;
  end;

  TTaskManager = class
  private
    FTasks: TObjectList<TLocalTask>;
    FCount: Integer;
    FOnChangeStatus: TTaskEvent;
    FIsWaitingTask: Boolean;   // true - если есть задачи ожидающие запуска
    function IsMemoryAvailable(RequiredMemoryMB: Cardinal): Boolean;
    function AreCPUCoresAvailable: Boolean;
    procedure FOnTaskStop;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddTask(const ID: integer; const AName: string; const ATaskProcedure: TProc);
    procedure StartTask(const AName: string); overload;
    procedure StartTask(const ID: integer); overload;
    procedure StopTask(const AName: string); overload;
    procedure StopTask(const ID: integer); overload;
    function FindTaskByName(const AName: string): TLocalTask;
    function FindTaskByID(const ID: Integer): TLocalTask;
    function GetStatus(const AName: string): String;
    property Count: Integer read FCount;
    property OnChangeStatus: TTaskEvent read FOnChangeStatus write FOnChangeStatus;
    property IsWaitingTask: Boolean read FIsWaitingTask;
  end;

implementation

{ TManagedTask }

constructor TLocalTask.Create(const ID: integer; const AName: string; const ATaskProcedure: TProc);
begin
  FName := AName;
  FID   := ID;
  FLock := TObject.Create;
  FShouldRun := False;
  FStopOnComplete := true;
  FTask := TTask.Create(
    procedure
    begin
      while True do
      begin
        TMonitor.Enter(FLock);
        try
          if not FShouldRun then
            Break;
        finally
          TMonitor.Exit(FLock);
        end;

        ATaskProcedure;

        if FStopOnComplete then Stop;

      end;
    end);
end;

destructor TLocalTask.Destroy;
begin
  Stop;
  FLock.Free;
  inherited;
end;

function TLocalTask.GetStatus: string;
begin
  Result := 'В ожидании';
  if FCompleting
    then Result := 'Завершена'
    else
      case FTask.Status of
        TTaskStatus.Created: Result := 'В ожидании';
        TTaskStatus.WaitingToRun,
        TTaskStatus.Running: Result := 'Выполняется';
        else Result := 'Завершена';
      end;

end;

procedure TLocalTask.Start;
begin
  TMonitor.Enter(FLock);
  try
    FShouldRun := True;
  finally
    TMonitor.Exit(FLock);
  end;

  if (FTask.Status = TTaskStatus.Created) then
      FTask.Start;

end;

procedure TLocalTask.Stop;
begin
  TMonitor.Enter(FLock);
  try
    FShouldRun := False;
  finally
    TMonitor.Exit(FLock);
  end;

  if Assigned(FTask) then
  begin
    //FTask.Wait;
    //FTask := nil;
  end;

  FCompleting := true;
  if Assigned(FOnTaskStop) then
    FOnTaskStop;


end;

function TLocalTask.IsRunning: Boolean;
begin
  TMonitor.Enter(FLock);
  try
    Result := FShouldRun;
  finally
    TMonitor.Exit(FLock);
  end;
end;

{ TTaskManager }

constructor TTaskManager.Create;
begin
  FTasks := TObjectList<TLocalTask>.Create(True);
  FIsWaitingTask := False;
  FCount := 0;
end;

destructor TTaskManager.Destroy;
var
  Task :TLocalTask;
begin
  {
  // не требуется секция, т.к. FTasks.Free - удаляет все элементы списка
  for Task in FTasks do
    if Assigned(Task) then
      Task.Destroy;
  }
  FTasks.Free;
  inherited;
end;

procedure TTaskManager.AddTask(const ID: integer; const AName: string; const ATaskProcedure: TProc);
var
  LocalTask: TLocalTask;
begin
  LocalTask := TLocalTask.Create(ID, AName, ATaskProcedure);

  if Assigned(FOnChangeStatus)
    then LocalTask.OnTaskStop := FOnTaskStop;

  FTasks.Add(LocalTask);

  inc(FCount);
end;

procedure TTaskManager.StartTask(const AName: string);
var
  Task: TLocalTask;
begin
  Task := FindTaskByName(AName);

  if IsMemoryAvailable(10) and AreCPUCoresAvailable
    then begin
      if Assigned(Task) then
        Task.Start
    end
    else begin

    end;

end;

procedure TTaskManager.StartTask(const ID: integer);
var
  Task: TLocalTask;
begin
  Task := FindTaskByID(ID);
  if Assigned(Task) then
    Task.Start;
end;

procedure TTaskManager.StopTask(const AName: string);
var
  Task: TLocalTask;
begin
  Task := FindTaskByName(AName);
  if Assigned(Task) then
    Task.Stop;
end;

procedure TTaskManager.StopTask(const ID: integer);
var
  Task: TLocalTask;
begin
  Task := FindTaskByID(ID);
  if Assigned(Task) then
    Task.Stop;
end;

function TTaskManager.FindTaskByID(const ID: Integer): TLocalTask;
var
  Task: TLocalTask;
begin

  for Task in FTasks do
    if Task.ID = ID then
      Exit(Task);

  Result := nil;
end;

function TTaskManager.FindTaskByName(const AName: string): TLocalTask;
var
  Task: TLocalTask;
begin

  for Task in FTasks do
    if Task.Name = AName then
      Exit(Task);

  Result := nil;
end;

function TTaskManager.GetStatus(const AName: string): String;
begin

  Result := 'Уточняется';
  if FindTaskByName(AName) <> nil
    then Result := FindTaskByName(AName).GetStatus;

end;

function TTaskManager.IsMemoryAvailable(RequiredMemoryMB: Cardinal): Boolean;
var
  MemStatus: TMemoryStatus;
begin
  MemStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MemStatus);

  // Проверяем, доступно ли больше оперативной памяти, чем требуется
  Result := (MemStatus.dwAvailPhys > RequiredMemoryMB * 1024 * 1024);
end;

procedure TTaskManager.FOnTaskStop;
begin
  if Assigned(FOnChangeStatus)
    then begin
      FOnChangeStatus;

      //можно организовать логику запуска задач, которые стоят в ожидании ресурсов
      //if FIsWaitingTask then

    end;

end;

function TTaskManager.AreCPUCoresAvailable: Boolean;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);

  // Предположим, что приложение рассматривает наличие хотя бы одного свободного ядра
  // (дополнительно можно усложнить проверку через использование System.Diagnostics для лучшего результата).
  // Здесь просто возвращаем True для примера.
  Result := True;
end;

end.
