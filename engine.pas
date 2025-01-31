unit engine;

{$mode ObjFPC}{$H+}{$J-}

interface


uses
  Classes, SysUtils, Sdl2;

type
  TEngine = class
  private
    AWindow: PSDL_Window;
    ARenderer: PSDL_Renderer;
    Running: boolean;
    ASdlKeyboardState: PUInt8;

  public
    constructor Create;
    destructor Destroy; override;
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
    WriteLn('ERROR::MAIN::Failed to Init Window');
    Exit;
  end;

  ARenderer := SDL_CreateRenderer(AWindow, -1, SDL_RENDERER_ACCELERATED or
    SDL_RENDERER_PRESENTVSYNC);
  if ARenderer = nil then
  begin
    WriteLn('ERROR::MAIN::Failed to Init Renderer');
    Exit;
  end;

  Running := True;

end;


destructor TEngine.Destroy;
begin

  {---------------------------------------------------------------------------}
  { Gabbage Collection                                                        }
  {---------------------------------------------------------------------------}

  if Assigned(ARenderer) then
  begin
    WriteLn('Destroy Renderer');
    SDL_DestroyRenderer(ARenderer);
  end;

  if Assigned(AWindow) then
  begin
    Writeln('Destroy Window');
    SDL_DestroyWindow(AWindow);
  end;

end;

procedure TEngine.GameLoop;
begin

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  WriteLn('Show Screen');
  ASdlKeyboardState := SDL_GetKeyboardState(nil);
  while Running = True do
  begin
    SDL_PumpEvents;

    // QUIT Check
    if ASdlKeyboardState[SDL_SCANCODE_ESCAPE] = 1 then
    begin
      WriteLn('Escape From Event Loop');
      Running := False;
    end;
    SDL_RenderClear(ARenderer);
    SDL_RenderPresent(ARenderer);
    SDL_Delay(20);

  end;
end;

end.
