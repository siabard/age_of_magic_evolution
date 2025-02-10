program magicevo;
{$mode objfpc}{$h+}{$j-}
uses
  SysUtils,
  sdl2,
  sdl2_image,
  sdl2_mixer,
  engine,
  asset_manager,
  LogUtil,
  hangul, textbox;

var
  AEngine: TEngine;
  Sample: string;
  Converted: TUTF16Array;
  I: integer;
  UniString: string;
  jaso: TJaso;
  bul: TBul;
begin

  {---------------------------------------------------------------------------}
  { Init SDL                                                                  }
  {---------------------------------------------------------------------------}
  LogDebug('INIT SDL2');
  if SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO) < 0 then
  begin
    LogDebug('ERROR::MAIN::Failed to Init SDL2');
    Exit;
  end;



  {---------------------------------------------------------------------------}
  { Engin 초기화                                                              }
  {---------------------------------------------------------------------------}
  AEngine := TEngine.Create;
  AEngine.GameInit('resources/asset.txt');

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  AEngine.GameLoop;

  {---------------------------------------------------------------------------}
  { Shutting down video / audio subsystem                                     }
  {---------------------------------------------------------------------------}

  FreeAndNil(AEngine);

  SDL_Quit;

end.
