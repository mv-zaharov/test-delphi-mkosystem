unit TaskManager;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Generics.Collections;

type
  TTaskEvent = procedure of object;

  TLocalTask = class
  private
    FTask: ITask;
    FStopOnComplete: Boolean;
    FShouldRun: Boolean;
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
    property StopOnComplete: boolean read FStopOnComplete;
    property OnTaskStop: TTaskEvent read FOnTaskStop write FOnTaskStop;
  end;

  TTaskManager = class
  private
    FTasks: TObjectList<TLocalTask>;
    FCount: Integer;
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
    property Count: Integer read FCount;
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

  FTasks.Add(LocalTask);
  inc(FCount);
end;

procedure TTaskManager.StartTask(const AName: string);
var
  Task: TLocalTask;
begin
  Task := FindTaskByName(AName);
  if Assigned(Task) then
    Task.Start;
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

end.
