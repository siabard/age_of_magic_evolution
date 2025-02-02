unit engine;

{$mode ObjFPC}{$H+}{$J-}

interface


uses
  Classes, SysUtils, Sdl2, asset_manager, LogUtil;

type
  TEngine = class
  private
    AWindow: PSDL_Window;
    ARenderer: PSDL_Renderer;
    Running: boolean;
    ASdlKeyboardState: PUInt8;
    AAssetManager: TAssetManager;

  public
    constructor Create;
    destructor Destroy; override;
    procedure GameInit(ConfigPath: PChar);
    procedure GameLoop;
  end;


implementation

constructor TEngine.Create;
begin
  {---------------------------------------------------------------------------}
  { Window / Renderer 를 초기화하기                                           }
  {---------------------------------------------------------------------------}
  AWindow := SDL_CreateWindow('Age of Magic Engine', 0, 0, 640, 480,
    SDL_WINDOW_SHOWN);

  if AWindow = nil then
  begin
    LogDebug('ERROR::MAIN::Failed to Init Window');
    Exit;
  end;

  ARenderer := SDL_CreateRenderer(AWindow, -1, SDL_RENDERER_ACCELERATED or
    SDL_RENDERER_PRESENTVSYNC);
  if ARenderer = nil then
  begin
    LogDebug('ERROR::MAIN::Failed to Init Renderer');
    Exit;
  end;

  AAssetManager := TAssetManager.Create(ARenderer);

  Running := True;

end;


destructor TEngine.Destroy;
begin

  {---------------------------------------------------------------------------}
  { Gabbage Collection                                                        }
  {---------------------------------------------------------------------------}

  AAssetManager.Free;

  if Assigned(ARenderer) then
  begin
    LogDebug('Destroy Renderer');
    SDL_DestroyRenderer(ARenderer);
  end;

  if Assigned(AWindow) then
  begin
    LogDebug('Destroy Window');
    SDL_DestroyWindow(AWindow);
  end;

end;

{------------------------------------------------------------------------------}
{ 환경설정 파일 읽어 들이기                                                    }
{------------------------------------------------------------------------------}
procedure TEngine.GameInit(ConfigPath: PChar);
var
  configFile: TextFile;
  config: string;
  Fields: TStringList;
begin
  Fields := TStringList.Create;

  AssignFile(configFile, ConfigPath);

  try
    Reset(configFile);
    try
      while not EOF(configFile) do
      begin
        ReadLn(configFile, config);

        // Config를 Split 한다.
        Fields.DelimitedText := config;

        if Fields[0] = 'texture' then
        begin
          // Texture Loading
          AAssetManager.LoadTexture(Fields[0], pansichar(Fields[1]));
        end;
      end;

    except
      on E: EInOutError do
      begin

        LogDebug('Failed Open File');

      end;
    end;




  finally
    CloseFile(configFile);
  end;

end;

procedure TEngine.GameLoop;
var
  itemTexture: PSDL_Texture;
begin

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  LogDebug('Show Screen');
  ASdlKeyboardState := SDL_GetKeyboardState(nil);
  while Running = True do
  begin
    SDL_PumpEvents;

    // QUIT Check
    if ASdlKeyboardState[SDL_SCANCODE_ESCAPE] = 1 then
    begin
      LogDebug('Escape From Event Loop');
      Running := False;
    end;
    SDL_RenderClear(ARenderer);

    itemTexture := AAssetManager.GetTexture('texture');
    if itemTexture <> nil then
      SDL_RenderCopy(ARenderer, itemTexture, nil, nil);
    SDL_RenderPresent(ARenderer);
    SDL_Delay(20);

  end;
end;

end.
